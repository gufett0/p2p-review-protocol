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
--import           Control.Monad.Trans.Either  --- ADDED BY ME
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String, (/))
import qualified Prelude

 

data Reviewing
instance Scripts.ValidatorTypes Reviewing where
    type instance DatumType Reviewing      = PaperDatum
    type instance RedeemerType Reviewing   = PaperRedeemer    

------------------ Validator --------------------------------------------------------------------------------------

{-# INLINABLE mkPaperValidator #-}
mkPaperValidator :: Paper -> PaperDatum -> PaperRedeemer -> ScriptContext -> Bool 
mkPaperValidator paper dat red ctx = 
    traceIfFalse "paper Tokens missing from UTXO-input" (assetClassValueOf (txOutValue ownInput) (paperNFT paper) == 2) &&
    case (dat, red) of 

        --reviewer finds an unreviewed paper.
        --paper review is ONGOING (reviewer votes either Minor or Major).
        (PaperDatum
               { d_currentManuscript  = Manuscript ipns
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = Nothing 
               , d_nextDeadline       = time 
               , d_status             = Submitted (Round r)
               }, Revision paperdecision) ->
            traceIfFalse "Error: Input   (Reviewer) - Not signed"             (txSignedBy info (unPaymentPubKeyHash rev_pkh)) && 
            traceIfFalse "Error: Input   (Script)   - Invalid Value"          (lovelaces (txOutValue ownInput) >= stake paper)         &&
            traceIfFalse "Error: Output  (Script)   - Invalid Value"          (lovelaces (txOutValue ownOutput) == (2*stake paper))  &&
            traceIfFalse "Error: Input   (Time)     - Too late"               (to (time) `contains` txInfoValidRange info)  &&
            traceIfFalse "Error: Output  (Script)   - Missing Tokens"         (assetClassValueOf (txOutValue ownOutput) (paperNFT paper) == 2) &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outputDatum == PaperDatum
                                                                                { d_currentManuscript  = Manuscript ipns
                                                                                , d_reviewerPkh        = rev_pkh
                                                                                , d_currentDecision    = Just paperdecision 
                                                                                , d_nextDeadline       = (time) + (timeInterval paper)
                                                                                , d_status             = Reviewed (Round (r + 1))
                                                                                })                                                       
        --author sees Minor or Major 
        --paper review is ONGOING (author updates the paper).
        (PaperDatum
               { d_currentManuscript  = Manuscript ipns
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = notfinal 
               , d_nextDeadline       = time 
               , d_status             = Reviewed (Round r)
               }, UpdatedAt ms) ->     
            traceIfFalse "Error: Input   (Author)   - Not signed"             (txSignedBy info (unPaymentPubKeyHash $ author paper)) && 
            traceIfFalse "Error: Input   (Script)   - Invalid Value"          (lovelaces (txOutValue ownInput) == (2*stake paper))   &&
            traceIfFalse "Error: Output  (Script)   - Invalid Value"          (lovelaces (txOutValue ownOutput) == (2*stake paper))  &&
            traceIfFalse "Error: Input   (Time)     - Too late"               (to (time) `contains` txInfoValidRange info)  &&
            traceIfFalse "Error: Input   (Redeemer) - Wrong paper referenced" (ms == (d_currentManuscript dat)) &&                   
            traceIfFalse "Error: Output  (Script)   - Missing Tokens"         (assetClassValueOf (txOutValue ownOutput) (paperNFT paper) == 2) &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outputDatum == PaperDatum
                                                                                { d_currentManuscript  = Manuscript ipns
                                                                                , d_reviewerPkh        = rev_pkh
                                                                                , d_currentDecision    = notfinal
                                                                                , d_nextDeadline       = (time) + (timeInterval paper)
                                                                                , d_status             = Submitted (Round r)
                                                                                })              
        --author sees Accept or Reject
        --paper review gets CLOSED (author claims half of his stake back).
        (PaperDatum
               { d_currentManuscript  = Manuscript ipns
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = final 
               , d_nextDeadline       = lastdeadline 
               , d_status             = Reviewed (Round r)
               }, ClosedAt ms) ->
            traceIfFalse "Error: Input   (Author)   - Not signed"             (txSignedBy info (unPaymentPubKeyHash $ author paper)) && 
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) == (2*stake paper))  &&
            traceIfFalse "Error: Output  (Script)   - Invalid value"          (lovelaces (txOutValue ownOutput) == ((stake paper) + (reward paper)))  &&
            traceIfFalse "Error: Input   (Time)     - Too late"               (to (lastdeadline) `contains` txInfoValidRange info) &&           
            traceIfFalse "Error: Input   (Redeemer) - Wrong paper referenced" (ms == (d_currentManuscript dat)) &&                   
            traceIfFalse "Error: Output  (Script)   - Missing tokens"         (assetClassValueOf (txOutValue ownOutput) (paperNFT paper) == 2) &&            
            traceIfFalse "Error: Input   (Datum)    - Not a final decision"   (final == (Just Accept) || final == (Just Reject)) &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outputDatum == PaperDatum
                                                                                { d_currentManuscript  = Manuscript ipns
                                                                                , d_reviewerPkh        = rev_pkh
                                                                                , d_currentDecision    = final 
                                                                                , d_nextDeadline       = lastdeadline
                                                                                , d_status             = Closed (Round r)
                                                                                })
        --reviewer sees a Closed status
        --paper review is OVER (reviewer claims his stake plus the reward).
        (PaperDatum
               { d_currentManuscript  = Manuscript _
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = _
               , d_nextDeadline       = _ 
               , d_status             = Closed _
               }, ClaimReviewer) ->
            traceIfFalse "Error: Input   (Reviewer) - Not signed"             (txSignedBy info (unPaymentPubKeyHash rev_pkh)) && 
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) == ((stake paper) + (reward paper)))  &&
            traceIfFalse "Error: Output  (Script)   - Missing token"          (assetClassValueOf (txOutValue ownOutput) (paperNFT paper) == 1) &&
            traceIfFalse "Error: Output  (Author)   - Missing token"           singleTokenToAuthor &&
            traceIfFalse "Error: Output  (Datum)    - Wrong output datum"     (outputDatum == dat)

        --author did not update or close in time
        --paper review is OVER (reviewer claims both stakes).
        (PaperDatum
               { d_currentManuscript  = Manuscript _
               , d_reviewerPkh        = rev_pkh
               , d_currentDecision    = _
               , d_nextDeadline       = time 
               , d_status             = Reviewed _
               }, ClaimReviewer) ->
            traceIfFalse "Error: Input   (Reviewer) - Not signed"             (txSignedBy info (unPaymentPubKeyHash rev_pkh)) &&
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) == (2*stake paper))  &&
            traceIfFalse "Error: Input   (Time)     - Within deadline"        (from (1 + time) `contains` txInfoValidRange info) &&
            traceIfFalse "Error: Output  (Author)   - Missing tokens"          pairTokensToAuthor 
                       
        --reviewer did not review in time
        --paper review is OVER (author claims everything).
        (PaperDatum
               { d_currentManuscript  = Manuscript _
               , d_reviewerPkh        = _
               , d_currentDecision    = _
               , d_nextDeadline       = time 
               , d_status             = Submitted _
               }, ClaimAuthor) ->
            traceIfFalse "Error: Input   (Author)   - Not signed"             (txSignedBy info (unPaymentPubKeyHash $ author paper)) &&
            traceIfFalse "Error: Input   (Script)   - Invalid value"          (lovelaces (txOutValue ownInput) >= stake paper)  &&
            traceIfFalse "Error: Input   (Time)     - Too early"              (from (1 + time) `contains` txInfoValidRange info) &&
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

        outputDatum :: PaperDatum
        outputDatum = case paperDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
            Nothing -> traceError "paper output datum not found"
            Just d  -> d

        singleTokenToAuthor :: Bool
        singleTokenToAuthor = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ author paper) (paperNFT paper) == 1

        pairTokensToAuthor :: Bool
        pairTokensToAuthor = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ author paper) (paperNFT paper) == 2


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

