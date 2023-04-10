{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NumericUnderscores  #-}



module TxConstruction where

import           Types
import           ReviewContract
import           NFTMintingPolicy
import           Utils
import           Control.Monad          hiding (fmap)
import           Data.Map               as Map
import           Data.Text              (Text,pack)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                --hiding (singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada
import           Text.Printf            (printf)
import           Prelude                (Semigroup (..), String, Show, Int, show, fromIntegral, div)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import           Playground.Contract    (ToSchema)
import qualified PlutusTx.Builtins      as Builtins
import           Data.Monoid            (Last (..))
import           Plutus.V1.Ledger.Value as Value
    

--------Endpoints definition

type PaperSchema =
            Endpoint "paperSubmission" AuthorParams  
        .\/ Endpoint "reviewerAction" ReviewerParams
        .\/ Endpoint "authorAction" AuthorParams
        .\/ Endpoint "closeAction" CloseParams

endpoints :: Contract () PaperSchema Text ()
endpoints = awaitPromise (paperSubmission' `select` reviewerAction' `select` authorAction' `select` closeAction') >> endpoints
  where
    paperSubmission' = endpoint @"paperSubmission" paperSubmission
    reviewerAction' = endpoint @"reviewerAction" reviewerAction
    authorAction' = endpoint @"authorAction" authorAction
    closeAction' = endpoint @"closeAction" closeAction


-- *** FIRST Endpoint *** -----------------------------------------------------------------------------------------------------------------------

paperSubmission :: forall w s. AuthorParams -> Contract w s Text ()
paperSubmission up = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    now <- currentTime
    let paper = Paper 
                    { author         = pkh
                    , reward         = upReward up
                    , stake          = upStake up
                    , minNumPeers    = 3
                    , timeInterval   = upTimeToDeadline up
                    , paperNFT       = AssetClass (upCurrency up, upTokenName up)
                    }
        v    = lovelaceValueOf (upStake up) <> assetClassValue (paperNFT paper) 2
        time = now + (timeInterval paper)
        dat  = PaperDatum
               { d_linkToManuscript  = upManuscript up 
               , d_reviewerPkh        = upReviewer up
               , d_currentDecision    = Nothing 
               , d_nextDeadline       = Just time 
               , d_status             = Submitted (Round 0)
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False 
               }
        tx   = Constraints.mustPayToTheScript dat v  
    ledgerTx <- submitTxConstraints (typedPaperValidator paper) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "[author] submitted manuscript - set stake: " ++ show (upStake up) 
                        ++ " and datum: " ++ show dat


--- *** SECOND Endpoint *** -----------------------------------------------------------------------------------------------------------------------

data ReviewerParams = ReviewerParams
    { rpAuthor          :: PaymentPubKeyHash
    , rpStake           :: Integer
    , rpReward          :: Integer
    , rpNumPeers        :: Integer
    , rpTimeToDeadline  :: POSIXTime 
    , rpCurrency        :: CurrencySymbol
    , rpTokenName       :: TokenName
    , rpDecision        :: PaperDecision
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)  

reviewerAction :: forall w s. ReviewerParams -> Contract w s Text ()
reviewerAction rp = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    let paper = Paper 
                { author         = rpAuthor rp
                , reward         = rpReward rp
                , stake          = rpStake rp
                , minNumPeers    = rpNumPeers rp
                , timeInterval   = rpTimeToDeadline rp
                , paperNFT       = AssetClass (rpCurrency rp, rpTokenName rp)
                }
        addr  = paperAddress paper        
 
    m <- findPaperOutput paper addr (Just pkh)
    now <- currentTime
    case m of
        Just (oref, o, datum) -> do
            case d_status datum of
                Closed _ -> do
                    logInfo @String "[reviewer] Author has closed the paper. Claiming rewards...." 
                    let token     = assetClassValue (paperNFT paper) 1 
                    let lookups   = Constraints.unspentOutputs (Map.singleton oref o) <>
                                    Constraints.otherScript (paperValidator paper) <>
                                    Constraints.typedValidatorLookups (typedPaperValidator paper) 
                        tx        = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ClaimReviewer) <> 
                                    Constraints.mustPayToTheScript (datum) (token <> adaValueOf (getAda minAdaTxOut)) <> -- nft + final datum locked in the script
                                    Constraints.mustPayToPubKey (author paper) (token <> adaValueOf (getAda minAdaTxOut)) <>
                                    Constraints.mustValidateIn (from now)               
                    ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                    let tid = getCardanoTxId ledgerTx
                    void $ awaitTxConfirmed tid
                    logInfo @String $ "[reviewer] claimed rewards"
                    logInfo @String $ "[reviewer] Final Datum: " ++ show (datum)

                _ -> do
                    logInfo @String "[reviewer] a paper review is requested. Now checking the deadline..."
                    deadline <- checkDeadline (d_nextDeadline datum) 
                    case deadline of                                              
                        
                        Left e  -> do
                            logInfo @String e
                            let (Reviewed roundValue) = d_status datum
                            status <- checkStatus (d_status datum) (Reviewed roundValue)
                            case status of 
                                Left e  -> logInfo @String "Sorry, you passed the deadline to decide on the manuscript!"
                                Right _ -> do
                                    logInfo @String "[reviewer] Author did not update or close the manuscript in time (datum was not updated)."
                                    let tokens     = assetClassValue (paperNFT paper) 2                        
                                    let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                  Constraints.otherScript (paperValidator paper)
                                        tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimReviewer) <>
                                                  Constraints.mustPayToPubKey (author paper) (tokens <> adaValueOf (getAda minAdaTxOut)) <> -- nfts back without datum
                                                  Constraints.mustValidateIn (from now)
                                    ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                                    logInfo @String "[reviewer] reclaimed all stakes"                                  

                        Right _ -> do                            
                            let (Submitted roundValue) = d_status datum
                            status <- checkStatus (d_status datum) (Submitted roundValue)
                            case status of
                                Left e  -> logInfo @String "Wait. The Author is still on time to update the manuscript!"           
                                                            
                                Right _ -> do
                                    logInfo @String "[reviewer] Now building the tx with a new paper decision..."                      
                                    {-let x = case roundValue of
                                                (Round r) | r > 0 -> (lovelaceValueOf $ (stake paper)*2 ) 
                                                _ -> lovelaceValueOf (stake paper) -}
                                    let tokens = assetClassValue (paperNFT paper) 2
                                    --let v = x <> tokens
                                    let v       = let y = lovelaceValueOf (stake paper) in y <> y <> tokens
                                        time = case d_nextDeadline datum of -- the last deadline gets extended by the hardcoded amount
                                                    Just t -> t + (timeInterval paper)
                                                    Nothing -> 0
                                        rev       = rpDecision rp 
                                        up_status = updateStatus $ d_status datum -- this updates Round +1 
                                    let out_datum = datum{d_currentDecision = Just rev, d_nextDeadline = Just time, d_status = up_status} 
                                        lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                Constraints.otherScript (paperValidator paper) <>
                                                Constraints.typedValidatorLookups (typedPaperValidator paper)
                                        tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Revision rev) <> 
                                                Constraints.mustPayToTheScript (out_datum) v <> 
                                                Constraints.mustValidateIn (to (fromMaybe 0 (d_nextDeadline datum)-1))                                          
                                    ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                    let tid = getCardanoTxId ledgerTx 
                                    void $ awaitTxConfirmed tid
                                    logInfo @String $ "[reviewer] reviewer's decision: " ++ show (rev)
                                    logInfo @String $ "[reviewer] New Datum: " ++ show (out_datum)
                                    

                           
        _ -> logInfo @String "[reviewer] no review request found"


--- *** THIRD Endpoint *** -----------------------------------------------------------------------------------------------------------------------

data AuthorParams = AuthorParams
    { upReviewer        :: Maybe PaymentPubKeyHash
    , upStake           :: Integer
    , upReward          :: Integer
    , upNumPeers        :: Integer
    , upTimeToDeadline  :: POSIXTime -- e.g. POSIXTime {getPOSIXTime :: 5}
    , upCurrency        :: CurrencySymbol
    , upTokenName       :: TokenName
    , upManuscript      :: Manuscript -- e.g. Manuscript "/ipns/QmS3..4uVv "
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)  

authorAction :: forall w s. AuthorParams -> Contract w s Text ()
authorAction up = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    let paper = Paper 
                { author         = pkh
                , reward         = upReward up
                , stake          = upStake up
                , minNumPeers    = upNumPeers up
                , timeInterval   = upTimeToDeadline up
                , paperNFT       = AssetClass (upCurrency up, upTokenName up)
                }
        addr  = paperAddress paper         

    m <- findPaperOutput paper addr (upReviewer up)
    now <- currentTime
    case m of
        Just (oref, o, datum) -> do
            logInfo @String "[author] reviewing paper found. Now checking the deadline..."
            deadline <- checkDeadline (d_nextDeadline datum)
            case deadline of
                Left e  -> do
                    logInfo @String e
                    let (Submitted roundValue) = d_status datum
                    status <- checkStatus (d_status datum) (Submitted roundValue)
                    case status of 
                        Left ee  -> logInfo @String $ ee ++ " Sorry, you passed the deadline to update the manuscript!" 
                        
                        Right _ -> do
                            logInfo @String "[author] Reviewer did not review in time (datum was not updated)."
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                          Constraints.otherScript (paperValidator paper)
                                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimAuthor) <>
                                          Constraints.mustValidateIn (from now)
                            ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                            logInfo @String "[author] reclaimed all stakes"                      
                
                Right _ -> do
                    let (Reviewed roundValue) = d_status datum
                    status <- checkStatus (d_status datum) (Reviewed roundValue)  
                    case status of
                        Left e  -> do
                            logInfo @String e
                            logInfo @String "Wait. The Reviewer is still on time to decide on the manuscript!"                                                 
                                                                        
                        Right _ -> case (d_currentDecision datum) of

                            Just decision | (decision == Accept || decision == Reject) -> do
                                logInfo @String "[author] Now building the tx with CLOSED datum..."
                                let tokens     = assetClassValue (paperNFT paper) 2
                                let v         = let x = lovelaceValueOf $ ((reward paper) + ((reward paper) `div` 2)) in x <> x <> tokens -- Value to the script should be e.g. 25+12.5, that'll go to reviewer
                                    --No need for a new deadline here 
                                    up_status = let lastround = (\(Reviewed (Round r)) -> r) (d_status datum) in Closed (Round lastround) -- THIS KEEPS THE LAST ROUND!                                    
                                    ipns      = upManuscript up
                                let out_datum = datum{d_nextDeadline = Nothing, d_status = up_status} -- Other fields remain unchanged
                                    lookups   = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                Constraints.otherScript (paperValidator paper) <>
                                                Constraints.typedValidatorLookups (typedPaperValidator paper)
                                    tx        = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ClosedAt ipns) <>
                                                Constraints.mustPayToTheScript (out_datum) v <>  
                                                Constraints.mustValidateIn (to (fromMaybe 0 (d_nextDeadline datum)-1))                                                           
                                ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                let tid = getCardanoTxId ledgerTx
                                void $ awaitTxConfirmed tid
                                logInfo @String $ "[author] Paper Closed! " ++ show (v)
                                logInfo @String $ "[author] Final Datum: " ++ show (out_datum)


                            Just decision | (decision == Minor || decision == Major) -> do
                                logInfo @String "[author] Now building the tx with UPDATED datum..."
                                let tokens    = assetClassValue (paperNFT paper) 2
                                let v         = let x = lovelaceValueOf $ (1*stake paper) in x <> x <> tokens
                                    time = case d_nextDeadline datum of -- the last deadline gets extended by the hardcoded amount
                                                    Just t -> t + (timeInterval paper)
                                                    Nothing -> 0                                  
                                    up_status     = updateStatus $ d_status datum -- this changes from (Reviewed Round x) to (Submitted Round x)
                                    ipns          = upManuscript up
                                let out_datum = datum{d_currentDecision = Nothing, d_nextDeadline = Just time, d_status = up_status} 
                                    lookups   = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                Constraints.otherScript (paperValidator paper) <>
                                                Constraints.typedValidatorLookups (typedPaperValidator paper)
                                    tx        = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ UpdatedAt ipns) <> 
                                                Constraints.mustPayToTheScript (out_datum) v <>
                                                Constraints.mustValidateIn (to (fromMaybe 0 (d_nextDeadline datum)-1))                                                        
                                ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                let tid = getCardanoTxId ledgerTx
                                void $ awaitTxConfirmed tid
                                logInfo @String $ "[author] Paper Updated!"
                                logInfo @String $ "[author] New Datum: " ++ show (out_datum)                               
                            _ -> logInfo @String "[author] corrupted decision"
                                     
        _ -> do
            logInfo @String "[author] no revision request found "
            
----------------            
data CloseParams = CloseParams
    { ppAuthor          :: PaymentPubKeyHash
    , ppStake           :: Integer
    , ppReward          :: Integer
    , ppNumPeers        :: Integer
    , ppTimeToDeadline  :: POSIXTime
    , ppCurrency        :: CurrencySymbol
    , ppTokenName       :: TokenName
    , ppFinalManuscript :: Manuscript
    , ppFinalNFTName    :: TokenName
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)  

closeAction :: forall w s. CloseParams -> Contract w s Text ()
closeAction pp = do
    --pkh <- Plutus.Contract.ownPaymentPubKeyHash            
    let paper = Paper 
                { author         = ppAuthor pp
                , reward         = ppReward pp
                , stake          = ppStake pp
                , minNumPeers    = ppNumPeers pp
                , timeInterval   = ppTimeToDeadline pp
                , paperNFT       = AssetClass (ppCurrency pp, ppTokenName pp)
                }            
    -- find all the right utxos
    utxos <- findLockedPaperOutputs paper (paperAddress paper)
    utxos_a <- findAuthorUtxo paper
    -- check if utxos at paper script address >= min num of peers
    utxos_size <- checkOutputsNumber (minNumPeers paper) utxos 
    case utxos_size of 
        Left e -> do
            logInfo @String $ "UTxOs check failed: " ++ e
        Right num_rev -> do
            let inDatums = getListPaperDatum utxos
            logInfo @String "Checking if number of Accept decisions is greater than Reject decisions"
            majority <- checkPaperDecisions $ getRevPkhAndDecision inDatums
            case majority of 
                Right _ -> do
                    logInfo @String "Majority of peers deems the paper acceptable for publication"
                    logInfo @String "Now building the peer-reviewed tx"

                    let rev_list= getRevPkhAndDecision inDatums
                        tname   = ppFinalNFTName pp
                        orefs   = Map.keys utxos
                        orefs_a = Map.keys utxos_a
                        nft     = Value.singleton (curSymbol (head orefs_a) tname) tname 1
                        tokens  = assetClassValue (paperNFT paper) (num_rev * 2)
                        out_dat = PaperDatum{
                                d_linkToManuscript    = ppFinalManuscript pp,
                                d_reviewerPkh         = Nothing,  
                                d_currentDecision     = Nothing,
                                d_nextDeadline        = Nothing,
                                d_status              = Closed (Round 0),
                                d_allRevDecisions     = Just rev_list,
                                d_peerReviewed        = True 
                                }
                    let lookups = Constraints.unspentOutputs (utxos <> utxos_a) <>
                                Constraints.otherScript (paperValidator paper) <>
                                Constraints.typedValidatorLookups (typedPaperValidator paper) <>
                                Constraints.mintingPolicy (policy (head orefs_a) tname)      
                        tx      =  mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ PeerReviewed (ppFinalManuscript pp) ) | oref <- orefs] <>
                                   mconcat [Constraints.mustSpendPubKeyOutput oref | oref <- orefs_a] <>
                                Constraints.mustPayToTheScript (out_dat) (tokens <> adaValueOf (getAda minAdaTxOut)) <>
                                Constraints.mustMintValue nft
                                                           

                    ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                    let tid = getCardanoTxId ledgerTx
                    void $ awaitTxConfirmed tid
                    logInfo @String $ "Final Peer Reviewed Datum: " ++ show (out_dat)
                    logInfo @String $ "Final Peer Reviewed NFT: " ++ show (nft)
                Left e  -> do
                    logInfo @String $ e ++ " You should wait until more peers deem the manuscript acceptable for publication"

-----------