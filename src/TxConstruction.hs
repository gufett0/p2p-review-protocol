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
import           Utils
import           Control.Monad          hiding (fmap)
import            Control.Monad.Trans.Except
import           Data.Map               as Map
import           Data.Text              (Text,pack)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     as Constraints
import           Ledger.Ada             as Ada
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (Semigroup (..), String,uncurry,show)
import           Text.Printf            (printf)
import           Prelude                (Show)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import           Playground.Contract    (ToSchema)
import qualified PlutusTx.Builtins      as Builtins
import           Data.Monoid            (Last (..))
import           Plutus.V1.Ledger.Value


--------Endpoints definition

type PaperSchema =
            Endpoint "createPaper" CreateParams  
        .\/ Endpoint "reviewPaper" ReviewParams
        .\/ Endpoint "updatePaper" UpdateParams

endpoints :: Contract () PaperSchema Text ()
endpoints = awaitPromise (createPaper' `select` reviewPaper' `select` updatePaper') >> endpoints

  where
    createPaper' = endpoint @"createPaper" createPaper
    reviewPaper' = endpoint @"reviewPaper" reviewPaper
    updatePaper' = endpoint @"updatePaper" updatePaper

-- Helper function

findPaperOutput :: Paper -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, PaperDatum))
findPaperOutput paper = do
    utxos <- utxosAt $ paperAddress paper 
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- paperDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        return (oref, o, dat)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (paperNFT paper) == 2


-- *** FIRST Endpoint *** -----------------------------------------------------------------------------------------------------------------------

data CreateParams = CreateParams
    { cpReviewer        :: PaymentPubKeyHash
    , cpStake           :: Integer
    , cpReward          :: Integer    
    , cpTimeToDeadline  :: POSIXTime -- e.g. POSIXTime {getPOSIXTime :: 5}
    , cpCurrency        :: CurrencySymbol
    , cpTokenName       :: TokenName
    , cpManuscript      :: Manuscript -- e.g. Manuscript "/ipns/QmS3..4uVv " 
     } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

createPaper :: forall w s. CreateParams -> Contract w s Text ()
createPaper cp = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    now <- currentTime
    let paper = Paper 
                    { author         = pkh
                 --   , reviewer       = cpReviewer cp
                    , reward         = cpReward cp
                    , stake          = cpStake cp
                    , timeInterval   = cpTimeToDeadline cp
                    , paperNFT       = AssetClass (cpCurrency cp, cpTokenName cp)
                    }
        v    = lovelaceValueOf (cpStake cp) <> assetClassValue (paperNFT paper) 2
        time = now + (timeInterval paper)
        dat  = PaperDatum
               { d_currentManuscript  = cpManuscript cp -- e.g. Manuscript "/ipns/QmS3..4uVv "
               , d_reviewerPkh        = cpReviewer cp
               , d_currentDecision    = Nothing 
               , d_nextDeadline       = time 
               , d_status             = Submitted (Round 0)
               }
        tx   = Constraints.mustPayToTheScript dat v  
    ledgerTx <- submitTxConstraints (typedPaperValidator paper) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "[author] submitted manuscript - set stake: " ++ show (cpStake cp) 
                        ++ " and deadline: " ++ show (d_nextDeadline dat)


--- *** SECOND Endpoint *** -----------------------------------------------------------------------------------------------------------------------

data ReviewParams = ReviewParams
    { rpAuthor          :: PaymentPubKeyHash
    , rpStake           :: Integer
    , rpReward          :: Integer
    , rpTimeToDeadline  :: POSIXTime -- e.g. POSIXTime {getPOSIXTime :: 5}
    , rpCurrency        :: CurrencySymbol
    , rpTokenName       :: TokenName
    , rpDecision        :: PaperDecision -- e.g. Minor
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)  

reviewPaper :: forall w s. ReviewParams -> Contract w s Text ()
reviewPaper rp = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    let paper = Paper 
                { author         = rpAuthor rp
         --       , reviewer       = pkh
                , reward         = rpReward rp
                , stake          = rpStake rp
                , timeInterval   = rpTimeToDeadline rp
                , paperNFT       = AssetClass (rpCurrency rp, rpTokenName rp)
                }
        
    m <- findPaperOutput paper
    now <- currentTime
    case m of
        Just (oref, o, datum) -> do
            case d_status datum of
                Closed _ -> do
                    logInfo @String "[reviewer] Author has closed the paper. Claiming rewards...." 
                    let token     = assetClassValue (paperNFT paper) 1 -- claim what's left (e.g. 25+12.5) + recreates outputs with token 
                    --let v         = let x = lovelaceValueOf (rpStake rp) in x <> x <> token
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
                    logInfo @String "[reviewer] paper review request is still open. Now checking the deadline..."
                    --deadline <- runExceptT $ checkDeadline (d_nextDeadline datum)
                    deadline <- checkDeadline (d_nextDeadline datum) 
                    case deadline of                                              
                        
                        Left e  -> do
                            logInfo @String e
                            let (Reviewed roundValue) = d_status datum
                            --status <- runExceptT $ checkStatus (d_status datum) (Reviewed roundValue)
                            status <- checkStatus (d_status datum) (Reviewed roundValue)
                            case status of 
                                Left e  -> logInfo @String "Sorry, you passed the deadline to decide on the manuscript!"
                                Right _ -> do
                                    logInfo @String "[reviewer] Author did not update or close the manuscript on time."
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
                            
                            logInfo @String "[reviewer] Now checking the review status..."
                            let (Submitted roundValue) = d_status datum
                            --status <- runExceptT $ checkStatus (d_status datum) (Submitted roundValue)
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
                                        time      = (d_nextDeadline datum) + (timeInterval paper) -- the last deadline gets extended by the hardcoded amount
                                        rev       = rpDecision rp -- e.g. Just Major or Just Accept
                                        up_status = updateStatus $ d_status datum -- this updates Round +1 
                                    let out_datum = datum{d_currentDecision = Just rev, d_nextDeadline = time, d_status = up_status} 
                                        lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                Constraints.otherScript (paperValidator paper) <>
                                                Constraints.typedValidatorLookups (typedPaperValidator paper)
                                        tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Revision rev) <> 
                                                Constraints.mustPayToTheScript (out_datum) v <> 
                                                Constraints.mustValidateIn (to $ (d_nextDeadline datum)-1 )                                                    
                                    ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                    let tid = getCardanoTxId ledgerTx
                                    void $ awaitTxConfirmed tid
                                    logInfo @String $ "[reviewer] reviewer's decision: " ++ show (rev)
                                    logInfo @String $ "[reviewer] New Datum: " ++ show (out_datum)
                           
        _ -> logInfo @String "[reviewer] no review request found"


--- *** THIRD Endpoint *** -----------------------------------------------------------------------------------------------------------------------

data UpdateParams = UpdateParams
    { upReviewer        :: PaymentPubKeyHash
    , upStake           :: Integer
    , upReward          :: Integer
    , upTimeToDeadline  :: POSIXTime -- e.g. POSIXTime {getPOSIXTime :: 5}
    , upCurrency        :: CurrencySymbol
    , upTokenName       :: TokenName
    , upManuscript      :: Manuscript -- e.g. Manuscript "/ipns/QmS3..4uVv "
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)  

updatePaper :: forall w s. UpdateParams -> Contract w s Text ()
updatePaper up = do
    pkh <- Plutus.Contract.ownPaymentPubKeyHash
    let paper = Paper 
                { author         = pkh
        --        , reviewer       = upAuthor up
                , reward         = upReward up
                , stake          = upStake up
                , timeInterval   = upTimeToDeadline up
                , paperNFT       = AssetClass (upCurrency up, upTokenName up)
                }
        
    m <- findPaperOutput paper
    now <- currentTime
    case m of
        Just (oref, o, datum) -> do
            logInfo @String "[author] paper found. Now checking the deadline..."
            --deadline <- runEitherT $ checkDeadline (d_nextDeadline datum)
            deadline <- checkDeadline (d_nextDeadline datum)
            case deadline of
                Left e  -> do
                    logInfo @String e
                    let (Submitted roundValue) = d_status datum
                    --status <- runEitherT $ checkStatus (d_status datum) (Submitted _)
                    status <- checkStatus (d_status datum) (Submitted roundValue)
                    case status of 
                        Left e  -> logInfo @String "Sorry, you passed the deadline to update the manuscript!" 
                        
                        Right _ -> do
                            logInfo @String "[author] Reviewer did not review in time."
                            let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                                          Constraints.otherScript (paperValidator paper)
                                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimAuthor) <>
                                          Constraints.mustValidateIn (from now)
                            ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                            logInfo @String "[author] reclaimed all stakes"                      
                
                Right _ -> do
                    logInfo @String "[author] Now checking the review status..."
                    let (Reviewed roundValue) = d_status datum
                    --status <- runEitherT $ checkStatus (d_status datum) (Reviewed _)
                    status <- checkStatus (d_status datum) (Reviewed roundValue) 
 
                    case status of
                        Left e  -> do
                            logInfo @String e
                            logInfo @String "Wait. The Reviewer is still on time to decide on the manuscript!"                                                 
                                                
                        
                        Right _ -> case (d_currentDecision datum) of

                            Just decision | (decision == Accept || decision == Reject) -> do
                                logInfo @String "[author] Now building the tx with CLOSED datum..."
                                let tokens     = assetClassValue (paperNFT paper) 2
                                let v         = let x = lovelaceValueOf $ ((reward paper)) in x <> x <> tokens -- Value to the script should be e.g. 25+12.5, that'll go to reviewer
                                    --time      = now + (timeInterval paper) -NO NEED TO SET A NEW DEADLINE!
                                    up_status = let lastround = (\(Reviewed (Round r)) -> r) (d_status datum) in Closed (Round lastround) -- THIS KEEPS THE LAST ROUND!                                    
                                    ipns      = upManuscript up
                                let out_datum = datum{d_status = up_status} -- OTHER FIELDS REMAIN UNCHANGED
                                    lookups   = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                Constraints.otherScript (paperValidator paper) <>
                                                Constraints.typedValidatorLookups (typedPaperValidator paper)
                                    tx        = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ ClosedAt ipns) <>
                                                Constraints.mustPayToTheScript (out_datum) v <>  
                                                Constraints.mustValidateIn (to $ (d_nextDeadline datum)-1 )                                                   
                                ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                let tid = getCardanoTxId ledgerTx
                                void $ awaitTxConfirmed tid
                                logInfo @String $ "[author] Paper Closed! " ++ show (v)
                                logInfo @String $ "[author] Final Datum: " ++ show (out_datum)


                            Just decision | (decision == Minor || decision == Major) -> do
                                logInfo @String "[author] Now building the tx with UPDATED datum..."
                                let tokens     = assetClassValue (paperNFT paper) 2
                                let v         = let x = lovelaceValueOf $ (1*stake paper) in x <> x <> tokens
                                    time          = (d_nextDeadline datum) + (timeInterval paper) -- the last deadline gets extended by the hardcoded amount
                                    up_status     = updateStatus $ d_status datum -- this changes from (Reviewed Round x) to (Submitted Round x)
                                    ipns          = upManuscript up
                                let out_datum = datum{d_currentDecision = Nothing, d_nextDeadline = time, d_status = up_status} 
                                    lookups   = Constraints.unspentOutputs (Map.singleton oref o) <>
                                                Constraints.otherScript (paperValidator paper) <>
                                                Constraints.typedValidatorLookups (typedPaperValidator paper)
                                    tx        = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ UpdatedAt ipns) <> 
                                                Constraints.mustPayToTheScript (out_datum) v <>
                                                Constraints.mustValidateIn (to $ (d_nextDeadline datum)-1 )                                                    
                                ledgerTx <- submitTxConstraintsWith @Reviewing lookups tx
                                let tid = getCardanoTxId ledgerTx
                                void $ awaitTxConfirmed tid
                                logInfo @String $ "[author] Paper Updated!"
                                logInfo @String $ "[author] New Datum: " ++ show (out_datum)
                            _ -> do
                                logInfo @String "[author] Nothing???"    

               

        _ -> logInfo @String "[author] no paper review was found"

------------ 

