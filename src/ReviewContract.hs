{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
 



module ReviewContract where

import           Types
import           Utils 
import           Control.Monad        hiding (fmap)
import           Data.List            as List 
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless, show)

data Reviewing
instance Scripts.ValidatorTypes Reviewing where
    type instance DatumType Reviewing      = PaperDatum
    type instance RedeemerType Reviewing   = PaperRedeemer

------------------ Validator --------------------------------------------------------------------------------------

{-# INLINABLE mkPaperValidator #-}
mkPaperValidator :: Paper -> PaperDatum -> PaperRedeemer -> ScriptContext -> Bool 
mkPaperValidator paper dat red ctx = 

    traceIfFalse "missing or wrong amount of tokens as UTXO-input" validate_inputs

     where        
        info :: TxInfo
        info = scriptContextTxInfo ctx

        validate_inputs :: Bool
        validate_inputs               
            | oneScriptInputTwoTokens = mkReviewing paper dat red ctx
            | allScriptInputsOneToken info = mkPeerReviewed paper () red ctx
            | otherwise = False         

        oneScriptInputTwoTokens :: Bool
        oneScriptInputTwoTokens =
            let tokenValue = paperToken paper
                ownInputResolved = txOutValue $ txInInfoResolved $ case findOwnInput ctx of
                    Nothing -> traceError "paper input missing"
                    Just i  -> i
            in assetClassValueOf ownInputResolved tokenValue  == 2   

        allScriptInputsOneToken :: TxInfo -> Bool
        allScriptInputsOneToken txInfo =
            let authorAddr = pubKeyHashAddress (author paper) Nothing
                scriptInputs = PlutusTx.Prelude.filter (\input -> txOutAddress (txInInfoResolved input) /= authorAddr) (txInfoInputs txInfo)
                scriptInputsVal = PlutusTx.Prelude.map (txOutValue . txInInfoResolved) scriptInputs
                inputsHaveOnePaperToken = List.all (\val -> assetClassValueOf val (paperToken paper) == 1) scriptInputsVal
            in inputsHaveOnePaperToken



{-# INLINABLE mkPeerReviewed #-}
mkPeerReviewed :: Paper -> () -> PaperRedeemer -> ScriptContext -> Bool 
mkPeerReviewed paper _ red ctx = do

    let revDecision = getRevPkhAndDecision inDatums
    case (red) of
        (PeerReviewed (Manuscript bs)) ->

            traceIfFalse "Error: Input  (Author) - Not signed"                                       (txSignedBy info (unPaymentPubKeyHash $ author paper)) &&
            traceIfFalse "Error: Input  (Script) - Value is less than minimum based on num of peers" (getInputValue txIn >= (minInput paper)) &&
            traceIfFalse "Error: Output (Script) - No more than min Ada must be locked"              (lovelaces (txOutValue ownOutput) == minADA) &&
            traceIfFalse "Error: Output (Script) - Missing locked tokens"                            (allTokensToScript) &&
            traceIfFalse "Error: Output (Author) - Missing correct NFT mint"                         (checkNFTname bs) &&
            traceIfFalse "Error: Output (Datum)  - Wrong output datum"                               (outDatum == PaperDatum
                                                                                                        { d_linkToManuscript   = Manuscript bs
                                                                                                        , d_reviewerPkh        = Nothing
                                                                                                        , d_currentDecision    = Nothing
                                                                                                        , d_nextDeadline       = Nothing
                                                                                                        , d_status             = Closed (Round 0)
                                                                                                        , d_allRevDecisions    = Just revDecision
                                                                                                        , d_peerReviewed       = True 
                                                                                                        })                      

        _ -> 
            traceError   "Error: Input   (Red)  - Wrong Redeemer"    
     
     where

        info :: TxInfo
        info = scriptContextTxInfo ctx

        outDatum :: PaperDatum
        outDatum = case paperDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
                        Nothing -> traceError "paper output datum not found"
                        Just d  -> d        

        inDatums :: [Maybe PaperDatum]
        inDatums = paperDatum . (>>= flip findDatum info) <$> datHash

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "expected exactly one pape output" 

        txIn :: [TxInInfo]
        txIn = txInfoInputs info

        datHash :: [Maybe DatumHash]
        datHash = txOutDatumHash . txInInfoResolved <$> txIn

        allTokensToScript :: Bool
        allTokensToScript = assetClassValueOf (valueLockedBy info $ ownHash ctx) (paperToken paper) >= (minNumPeers paper*2)

        checkNFTname :: BuiltinByteString -> Bool
        checkNFTname bs = case flattenValue (txInfoMint info) of
            [(_, tname, amt)] -> amt == 1 && (unTokenName tname) == "PeerReviewedNFT." `appendByteString` bs


{-# INLINABLE mkReviewing #-}
mkReviewing :: Paper -> PaperDatum -> PaperRedeemer -> ScriptContext -> Bool 
mkReviewing paper dat red ctx = 
    
    case (dat, red) of 
        --reviewer finds an unreviewed paper.
        --paper review is ONGOING (reviewer votes).
        (PaperDatum
               { d_linkToManuscript   = Manuscript ipns
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = Nothing 
               , d_nextDeadline       = time 
               , d_status             = Submitted (Round r)
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False
               }, Revision paperdecision) ->
            traceIfFalse "Error: Input   (Reviewer) - Not signed"             (maybe False (txSignedBy info . unPaymentPubKeyHash) rev_pkh) && 
            traceIfFalse "Error: Input   (Script)   - Invalid Value"          (lovelaces (txOutValue ownInput) == if r == 0 then stake paper else 2*stake paper) && 
            traceIfFalse "Error: Output  (Script)   - Invalid Value"          (lovelaces (txOutValue ownOutput) == (2*stake paper))  &&
            traceIfFalse "Error: Input   (Time)     - Too late"               (maybe False (\t -> to (t - 1) `contains` txInfoValidRange info) time) &&
            traceIfFalse "Error: Output  (Script)   - Missing Tokens"         (assetClassValueOf (txOutValue ownOutput) (paperToken paper) == 2) &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outDatum == PaperDatum
                                                                                { d_linkToManuscript   = Manuscript ipns
                                                                                , d_reviewerPkh        = rev_pkh
                                                                                , d_currentDecision    = Just paperdecision 
                                                                                , d_nextDeadline       = fmap (\t -> t + timeInterval paper) time
                                                                                , d_status             = Reviewed (Round (r + 1))
                                                                                , d_allRevDecisions    = Nothing
                                                                                , d_peerReviewed       = False
                                                                                })                                                       
        --author sees Minor or Major 
        --paper review is ONGOING (author updates the paper).
        (PaperDatum
               { d_linkToManuscript   = Manuscript ipns
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = notfinal 
               , d_nextDeadline       = time 
               , d_status             = Reviewed (Round r)
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False
               }, UpdatedAt ms) ->     
            traceIfFalse "Error: Input   (Author)   - Not signed"             (txSignedBy info (unPaymentPubKeyHash $ author paper)) && 
            traceIfFalse "Error: Input   (Script)   - Invalid Value"          (lovelaces (txOutValue ownInput) == (2*stake paper))   &&
            traceIfFalse "Error: Output  (Script)   - Invalid Value"          (lovelaces (txOutValue ownOutput) == (2*stake paper))  &&
            traceIfFalse "Error: Input   (Time)     - Too late"               (maybe False (\t -> to (t - 1) `contains` txInfoValidRange info) time) &&
            traceIfFalse "Error: Input   (Redeemer) - Wrong paper referenced" (ms == (d_linkToManuscript dat)) &&                   
            traceIfFalse "Error: Output  (Script)   - Missing Tokens"         (assetClassValueOf (txOutValue ownOutput) (paperToken paper) == 2) &&
            traceIfFalse "Error: Input   (Datum)    - Decision is final"      (notfinal == (Just Minor) || notfinal == (Just Major)) &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outDatum == PaperDatum
                                                                                { d_linkToManuscript   = Manuscript ipns
                                                                                , d_reviewerPkh        = rev_pkh
                                                                                , d_currentDecision    = Nothing
                                                                                , d_nextDeadline       = fmap (\t -> t + timeInterval paper) time
                                                                                , d_status             = Submitted (Round r)
                                                                                , d_allRevDecisions    = Nothing
                                                                                , d_peerReviewed       = False
                                                                                })              
        --author sees Accept or Reject
        --paper review gets CLOSED (author claims only half of his stake back).
        (PaperDatum
               { d_linkToManuscript   = Manuscript ipns
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = final 
               , d_nextDeadline       = lastdeadline 
               , d_status             = Reviewed (Round r)
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False
               }, ClosedAt ms) ->
            traceIfFalse "Error: Input   (Author)   - Not signed"             (txSignedBy info (unPaymentPubKeyHash $ author paper)) && 
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) == (2*stake paper))  &&
            traceIfFalse "Error: Output  (Script)   - Invalid value"          (lovelaces (txOutValue ownOutput) == ((stake paper) + (compensation paper)))  &&
            traceIfFalse "Error: Input   (Time)     - Too late"               (maybe False (\t -> to (t - 1) `contains` txInfoValidRange info) lastdeadline) &&           
            traceIfFalse "Error: Input   (Redeemer) - Wrong paper referenced" (ms == (d_linkToManuscript dat)) &&                   
            traceIfFalse "Error: Output  (Script)   - Missing tokens"         (assetClassValueOf (txOutValue ownOutput) (paperToken paper) == 2) &&            
            traceIfFalse "Error: Input   (Datum)    - Not a final decision"   (final == (Just Accept) || final == (Just Reject)) &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outDatum == PaperDatum
                                                                                { d_linkToManuscript   = Manuscript ipns
                                                                                , d_reviewerPkh        = rev_pkh
                                                                                , d_currentDecision    = final 
                                                                                , d_nextDeadline       = Nothing
                                                                                , d_status             = Closed (Round r)
                                                                                , d_allRevDecisions    = Nothing
                                                                                , d_peerReviewed       = False
                                                                                })
        --reviewer sees a Closed status
        --paper review is OVER (reviewer claims his stake plus the compensation).
        (PaperDatum
               { d_linkToManuscript   = Manuscript _
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = _
               , d_nextDeadline       = _ 
               , d_status             = Closed _
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False
               }, ClaimReviewer) ->
            traceIfFalse "Error: Input   (Reviewer) - Not signed"             (maybe False (txSignedBy info . unPaymentPubKeyHash) rev_pkh) && 
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) == ((stake paper) + (compensation paper)))  &&
            traceIfFalse "Error: Output  (Script)   - Missing token"          (assetClassValueOf (txOutValue ownOutput) (paperToken paper) == 1) &&
            traceIfFalse "Error: Output  (Author)   - Missing token"           singleTokenToAuthor &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outDatum == dat)

        --author did not update or close in time
        --paper review is OVER (reviewer claims both stakes).
        (PaperDatum
               { d_linkToManuscript   = Manuscript _
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = _
               , d_nextDeadline       = time 
               , d_status             = Reviewed _
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False
               }, ClaimReviewer) ->
            traceIfFalse "Error: Input   (Reviewer) - Not signed"             (maybe False (txSignedBy info . unPaymentPubKeyHash) rev_pkh) &&
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) == (2*stake paper))  &&
            traceIfFalse "Error: Input   (Time)     - Still within deadline"  (maybe False (\t -> from t `contains` txInfoValidRange info) time) &&
            traceIfFalse "Error: Output  (Author)   - Missing tokens"          pairTokensToAuthor 
                       
        --reviewer did not review in time
        --paper review is OVER (author claims everything).
        (PaperDatum
               { d_linkToManuscript   = Manuscript _
               , d_reviewerPkh        = _
               , d_currentDecision    = _
               , d_nextDeadline       = time 
               , d_status             = Submitted _
               , d_allRevDecisions    = Nothing
               , d_peerReviewed       = False 
               }, ClaimAuthor) ->
            traceIfFalse "Error: Input   (Author)   - Not signed"             (txSignedBy info (unPaymentPubKeyHash $ author paper)) &&
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) >= stake paper)  &&
            traceIfFalse "Error: Input   (Time)     - Too early"              (maybe False (\t -> from (t + 1) `contains` txInfoValidRange info) time) &&
            traceIfFalse "Error: Output  (Author)   - Missing tokens"          pairTokensToAuthor 

        _                      ->
            traceError   "Error: Input   (Dat,Red)  - Wrong Datum Or Redeemer"          

     where

        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "paper input missing"
            Just i  -> txInInfoResolved i

        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "expected exactly one paper output" 

        outDatum :: PaperDatum
        outDatum = case paperDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
                        Nothing -> traceError "paper output datum not found"
                        Just d  -> d        

        singleTokenToAuthor :: Bool
        singleTokenToAuthor = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ author paper) (paperToken paper) == 1

        pairTokensToAuthor :: Bool
        pairTokensToAuthor = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ author paper) (paperToken paper) == 2


-------

typedPaperValidator :: Paper -> Scripts.TypedValidator Reviewing
typedPaperValidator paper = Scripts.mkTypedValidator @Reviewing
    ($$(PlutusTx.compile [|| mkPaperValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode paper)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @PaperDatum @PaperRedeemer

paperValidator :: Paper -> Validator
paperValidator = Scripts.validatorScript . typedPaperValidator

paperAddress :: Paper -> Ledger.Address
paperAddress = scriptAddress . paperValidator

