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
import           Data.Text              (Text)
import           Data.Map               as Map
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Prelude                (show, fromIntegral, String)
import           Ledger                 hiding (singleton)
import qualified Plutus.V1.Ledger.Ada   as Ada
import           Plutus.V1.Ledger.Value




{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE minADA #-}
minADA :: Integer
minADA = lovelaces (Ada.adaValueOf (Ada.getAda minAdaTxOut))

{-# INLINABLE minInput #-}
minInput :: Paper -> Integer
minInput p = minADA * (minNumPeers p)

{-# INLINABLE paperDatum #-}
paperDatum :: Maybe Datum -> Maybe PaperDatum
paperDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE getInputValue #-}
getInputValue :: [TxInInfo] -> Integer
getInputValue i =  sum input_list
    where 
        input_list = PlutusTx.Prelude.map (Ada.getLovelace . Ada.fromValue . txOutValue . txInInfoResolved) i

{-# INLINABLE getRevPkhAndDecision #-}
getRevPkhAndDecision :: [Maybe PaperDatum] -> [(PaymentPubKeyHash, PaperDecision)]
getRevPkhAndDecision datums =
  loop datums
  where
    loop [] = []
    loop (Just dat:dats) =
      case (d_reviewerPkh dat, d_currentDecision dat) of
        (Just reviewerPkh, Just decision) -> (reviewerPkh, decision) : loop dats
        _ -> loop dats
    loop (Nothing:dats) = loop dats          


getPaperDatum :: ChainIndexTxOut -> Maybe PaperDatum
getPaperDatum o = case _ciTxOutDatum o of
  Left _ -> Nothing
  Right d -> paperDatum $ Just d

checkStatus :: PaperStatus -> PaperStatus -> Contract w s Text (Either String ())
checkStatus status arg =
    if status == arg 
        then return (Right ())
        else return (Left "status does not match")

checkDeadline :: Maybe POSIXTime -> Contract w s Text (Either String ())
checkDeadline maybeDeadline = do
    now <- currentTime
    case maybeDeadline of
      Just deadline -> if now >= deadline 
                          then return (Left "Deadline Passed!") 
                          else return (Right ())
      Nothing -> return (Right ()) -- if there's no deadline, always return Right
      

updateStatus :: PaperStatus -> PaperStatus
updateStatus (Submitted (Round r)) = Reviewed (Round (r + 1))
updateStatus (Reviewed (Round r)) = Submitted (Round r)
updateStatus status = status


findAuthorUtxo :: Paper -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findAuthorUtxo paper = do
    utxos <- utxosAt $ pubKeyHashAddress (author paper) Nothing
    let filteredUtxos = Map.filter f utxos
    return filteredUtxos
  where
    f :: ChainIndexTxOut -> Bool
    f o = assetClassValueOf (_ciTxOutValue o) (paperNFT paper) >= 1 


findLockedPaperOutputs :: Paper -> Ledger.Address -> Contract w s Text (Map TxOutRef ChainIndexTxOut)
findLockedPaperOutputs paper addr = do
    utxos <- utxosAt $ addr
    let filteredUtxos = Map.filter f utxos
    return filteredUtxos
  where
    isClosed :: PaperDatum -> Bool
    isClosed d = case d_status d of
                    Closed _ -> True
                    _ -> False
    f :: ChainIndexTxOut -> Bool
    f o = let dat = getPaperDatum o
          in case dat of
                Just d -> assetClassValueOf (_ciTxOutValue o) (paperNFT paper) == 1 && isClosed d
                Nothing -> False


findPaperOutput :: Paper -> Ledger.Address -> Maybe PaymentPubKeyHash -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, PaperDatum))
findPaperOutput paper add revPkh = do
    utxos <- utxosAt $ add
    return $ do
        (oref, o) <- find f $ Map.toList utxos
        dat       <- getPaperDatum o
        return (oref, o, dat)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = let dat = getPaperDatum o
                in case dat of
                Just d -> assetClassValueOf (_ciTxOutValue o) (paperNFT paper) == 2 && (d_reviewerPkh d) == revPkh
                Nothing -> False


getListPaperDatum :: Map TxOutRef ChainIndexTxOut -> [Maybe PaperDatum]
getListPaperDatum utxos = loop $ Map.elems utxos
  where
    loop [] = []
    loop (txOut:txOuts) =
      case getPaperDatum txOut of
        Just datum -> Just datum : loop txOuts
        Nothing -> loop txOuts

checkOutputsNumber :: Integer -> Map TxOutRef ChainIndexTxOut -> Contract w s Text (Either String Integer)
checkOutputsNumber n utxos =
    if fromIntegral (Map.size utxos) >= n
        then return $ Right $ fromIntegral (Map.size utxos)
        else return $ Left $ "Expected at least " ++ show n ++ " UTxOs, but found only " ++ show (Map.size utxos)

checkPaperDecisions :: [(PaymentPubKeyHash, PaperDecision)] -> Contract w s Text (Either String Bool)
checkPaperDecisions decisions = do
  let acceptCount = length $ PlutusTx.Prelude.filter (\(_, d) -> d == Accept) decisions
      rejectCount = length $ PlutusTx.Prelude.filter (\(_, d) -> d == Reject) decisions
  if acceptCount > rejectCount
    then return $ Right True
    else return $ Left "Number of Accept decisions is not greater than Reject decisions"

