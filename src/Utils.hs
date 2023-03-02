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

module Utils where


import           Types
import           Control.Monad          hiding (fmap)
import           Data.Text              (Text,pack)
import           Data.Ratio
import           Plutus.Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap      as Map
import           PlutusTx.Prelude       hiding (Semigroup(..), String, unless)
import           Prelude                (show, Double, String)
import           Ledger                 hiding (singleton)
import qualified Plutus.V1.Ledger.Ada   as Ada
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet
import           Data.Monoid            (Last (..))
import           Plutus.Trace.Emulator  as Emulator




{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE paperDatum #-}
paperDatum :: Maybe Datum -> Maybe PaperDatum
paperDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

getPaperDatum :: ChainIndexTxOut -> Maybe PaperDatum
getPaperDatum o = case _ciTxOutDatum o of
  Left _ -> Nothing
  Right d -> paperDatum $ Just d

checkStatus :: PaperStatus -> PaperStatus -> Contract w s Text (Either String ())
checkStatus status arg =
    if status == arg 
        then return (Right ())
        else return (Left "status does not match")

checkDeadline :: POSIXTime -> Contract w s Text (Either String ())
checkDeadline deadline = do
    now <- currentTime
    if now >= deadline 
        then return (Left "Deadline Passed!") 
        else return (Right ())        


updateStatus :: PaperStatus -> PaperStatus
updateStatus (Submitted (Round round)) = Reviewed (Round (round + 1))
updateStatus (Reviewed (Round round)) = Submitted (Round round)
updateStatus status = status

