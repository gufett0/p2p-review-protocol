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
 

module ReviewPolicy where

import           Types
import           Utils
import           ReviewContract 
import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import           Plutus.V1.Ledger.Credential(StakingCredential(..) , Credential(..))


{-# INLINABLE nftMintingPolicy  #-}
nftMintingPolicy :: Address -> TxOutRef -> TokenName -> () -> ScriptContext -> Bool
nftMintingPolicy addr oref tname () ctx = traceIfFalse "UTxO not consumed" hasUTxO &&
                                    traceIfFalse "Wrong ammount minted" checkMintedAmount &&
                                    traceIfFalse "Inputs from script address are not being validated" (scriptInput /= Nothing)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    
    txIn :: [TxInInfo]
    txIn = txInfoInputs info

    hasUTxO :: Bool
    hasUTxO =  any (\utxo -> txInInfoOutRef utxo == oref) $ txIn

    checkMintedAmount :: Bool
    checkMintedAmount =  case flattenValue (txInfoMint info) of
        [(_,tname', amount)]  -> tname' == tname && amount == 1
        _                     -> False
 
    scriptInput :: Maybe TxInInfo
    scriptInput = case find (isScriptAddress . txOutAddress . txInInfoResolved) txIn of
                    Just i -> Just i
                    Nothing -> Nothing

    isScriptAddress :: Address -> Bool
    isScriptAddress (Address { addressCredential = ScriptCredential vh
                            , addressStakingCredential = Nothing
                            }) = Just vh == (toValidatorHash addr)
    isScriptAddress _ = False



policy :: Address -> TxOutRef -> TokenName -> Scripts.MintingPolicy
policy add oref tname = mkMintingPolicyScript $
             $$(PlutusTx.compile [|| \add' oref' tname' -> Scripts.wrapMintingPolicy $ nftMintingPolicy add' oref' tname' ||])
             `PlutusTx.applyCode`
             PlutusTx.liftCode add
             `PlutusTx.applyCode`
             PlutusTx.liftCode oref
             `PlutusTx.applyCode`
             PlutusTx.liftCode tname

curSymbol :: Address -> TxOutRef -> TokenName -> CurrencySymbol
curSymbol add oref tname = scriptCurrencySymbol $ policy add oref tname    