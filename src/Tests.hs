{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TupleSections         #-} --added by me
 
module Tests ( test, test', PaperDatum (..), PaperDecision (..), paperDatum ) where

import           Types
import           Utils
import           TxConstruction
import           ReviewContract         
import           Control.Monad              hiding (fmap)
import           Control.Lens --added by me
--import           Ledger.Tx (CardanoTx(..), lookupDatum)--added by me for lookupDatum 
--import           Plutus.Contract      as Contract --added by me
import           Data.Maybe --added by me for lookupDatum
--import           Wallet.Emulator.Chain --added by me
--import           Control.Monad.Freer (Eff, Member, interpret, runM) --added by me
--import           Plutus.ChainIndex.Effects() --added by me
--import           Plutus.ChainIndex.Tx() --added by me
--import           Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx))--added by me

import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot()
import           Ledger.Value 
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..))
import           Wallet.Emulator.Wallet



paperReviewTokenCurrency :: CurrencySymbol
paperReviewTokenCurrency = "2023"

paperReviewToken :: TokenName
paperReviewToken = "PaperToken"

w1, w2 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2

test :: IO ()
test = do
    test' Minor Accept
    test' Major Accept
    test' Major Reject

test' :: PaperDecision -> PaperDecision -> IO ()
test' pd1 pd2 = runEmulatorTraceIO' def emCfg $ myTrace pd1 pd2

  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue (AssetClass (paperReviewTokenCurrency, paperReviewToken)) 2)
                    , (w2, v)
                    ]
                }

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

---------------------------

myTrace :: PaperDecision -> PaperDecision -> EmulatorTrace ()
myTrace pd1 pd2 = do
    Extras.logInfo $ "Simulating just a " ++ show pd1 ++ "revision with a final " ++ show pd2 ++ "decision"

    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints

    let mypaper = Paper { 
      author           = mockWalletPaymentPubKeyHash w1
    , stake            = 100_000_000
    , reward           = 50_000_000
    , timeInterval     = POSIXTime {getPOSIXTime = 10_000}
    , paperNFT         = (AssetClass (paperReviewTokenCurrency, paperReviewToken))
    } 

    let pkh_rev   = mockWalletPaymentPubKeyHash w2
        fileIpns  = Manuscript "ipns_address"
        scriptAdd = paperAddress mypaper 
        --deadline2 = slotToBeginPOSIXTime def 10
        

        cp = CreateParams 
                { cpReviewer        = pkh_rev
                , cpStake           = stake mypaper
                , cpReward          = reward mypaper
                , cpTimeToDeadline  = timeInterval mypaper
                , cpCurrency        = paperReviewTokenCurrency
                , cpTokenName       = paperReviewToken
                , cpManuscript      = fileIpns
                }
  
        rp1 = ReviewParams
                { rpAuthor         = author mypaper
                , rpStake          = stake mypaper
                , rpReward         = reward mypaper
                , rpTimeToDeadline = timeInterval mypaper
                , rpCurrency       = paperReviewTokenCurrency
                , rpTokenName      = paperReviewToken
                , rpDecision       = pd1
                }

        rp2 = rp1{rpDecision       = pd2}

        up1 = UpdateParams
                { upReviewer        = pkh_rev
                , upStake           = stake mypaper
                , upReward          = reward mypaper
                , upTimeToDeadline  = timeInterval mypaper
                , upCurrency        = paperReviewTokenCurrency
                , upTokenName       = paperReviewToken
                , upManuscript      = fileIpns
                }

        
    callEndpoint @"createPaper" h1 cp

    void $ Emulator.waitNSlots 5

    callEndpoint @"reviewPaper" h2 rp1

    void $ Emulator.waitNSlots 5

    callEndpoint @"updatePaper" h1 up1

    void $ Emulator.waitNSlots 5

    callEndpoint @"reviewPaper" h2 rp2

    void $ Emulator.waitNSlots 5

    callEndpoint @"updatePaper" h1 up1

------------

    finalUtxos <- getUtxos scriptAdd
    Extras.logInfo $ "Final Script UTXOS: " ++ show finalUtxos

    finalUtxosPB <- getUtxos (mockWalletAddress w1)
    Extras.logInfo $ "Final Author UTXOS: " ++ show finalUtxosPB



getUtxos :: Address -> Emulator.EmulatorTrace [(TxOutRef, ChainIndexTxOut)]
getUtxos addr = do
    state <- Emulator.chainState
    let utxoIndex = getIndex $ state ^. Emulator.index 
        utxos     = Data.Maybe.mapMaybe (\(oref, o) -> (oref, ) <$> 
                    fromTxOut o) [(oref, o) | (oref, o) <- Map.toList utxoIndex, txOutAddress o == addr]
    return utxos


findMyDat :: Address -> Emulator.EmulatorTrace (Maybe PaperDatum)
findMyDat addr = do
    utxos <- getUtxos addr
    return $ do
        --(oref, o) <- find f $ Map.toList utxos
        (_, o) <- find f $ utxos
        dat       <- paperDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        --return $ Data.Maybe.catMaybes (dat)
        return dat
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (AssetClass (paperReviewTokenCurrency, paperReviewToken)) == 1