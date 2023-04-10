{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TupleSections         #-} 
 
module Tests where

import           Types
import           Utils
import           TxConstruction
import           ReviewContract         
import           Control.Monad              hiding (fmap)
import           Control.Lens 
import           Data.Maybe 
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Ledger
import           Ledger.TimeSlot()
import           Ledger.Value 
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..), String, div, last)
import           Wallet.Emulator.Wallet
{-
import           Cardano.Wallet.Api
import Ledger.Index 
import           Plutus.Contract -}


paperReviewTokenCurrency :: CurrencySymbol
paperReviewTokenCurrency = "2023"

paperReviewToken :: TokenName
paperReviewToken = "PaperReviewToken"

{-
peerIdTokenCurrency :: CurrencySymbol
peerIdTokenCurrency = "20231"

peerIdToken01, peerIdToken02, peerIdToken03 :: TokenName
peerIdToken01 = "PeerIdentityToken01"
peerIdToken02 = "PeerIdentityToken02"
peerIdToken03 = "PeerIdentityToken03"
-}

w1, w2, w3, w4 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3
w4 = knownWallet 4


testSet :: IO ()
testSet = do
    test [[Minor, Accept], [Major, Reject], [Major, Minor, Accept]]
    --test [[Minor, Reject], [Minor, Reject], [Major, Minor, Accept]]


test :: [[PaperDecision]] -> IO ()
test pdlist = runEmulatorTraceIO' def emCfg $ myTrace pdlist
  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue (AssetClass (paperReviewTokenCurrency, paperReviewToken)) 6)
                    , (w2, v )
                    , (w3, v )
                    , (w4, v )
                    ]
                }

    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

---------------------------

myTrace :: [[PaperDecision]] -> EmulatorTrace ()
myTrace pdlist = do
    Extras.logInfo $ "Simulating revisions with decisions: " ++ show pdlist

    h0 <- activateContractWallet w1 endpoints
    h1 <- activateContractWallet w2 endpoints
    h2 <- activateContractWallet w3 endpoints
    h3 <- activateContractWallet w4 endpoints

    let mypaper = Paper { 
      author           = mockWalletPaymentPubKeyHash w1
    , stake            = 100_000_000
    , reward           = stake mypaper `div` 2
    , minNumPeers      = 3
    , timeInterval     = POSIXTime {getPOSIXTime = 10_000}
    , paperNFT         = (AssetClass (paperReviewTokenCurrency, paperReviewToken))
    } 

    let pkh_reviewer1   = mockWalletPaymentPubKeyHash w2
        pkh_reviewer2   = mockWalletPaymentPubKeyHash w3
        pkh_reviewer3   = mockWalletPaymentPubKeyHash w4
        fileIpns  = Manuscript "/ipns/QmS4ust...4uVv"
        finalNFT  = "PeerReviewedNFT./ipns/QmS4ust...4uVv"
        scriptAdd = paperAddress mypaper      
        ap0 = AuthorParams 
                        { upReviewer        = Just pkh_reviewer1
                        , upStake           = stake mypaper
                        , upReward          = reward mypaper
                        , upNumPeers        = minNumPeers mypaper
                        , upTimeToDeadline  = timeInterval mypaper
                        , upCurrency        = paperReviewTokenCurrency
                        , upTokenName       = paperReviewToken
                        , upManuscript      = fileIpns
                        }
        ap2 = ap0{upReviewer = Just pkh_reviewer2}
        ap3 = ap0{upReviewer = Just pkh_reviewer3}
        rp = ReviewerParams
                        { rpAuthor         = author mypaper
                        , rpStake          = stake mypaper
                        , rpReward         = reward mypaper
                        , rpNumPeers       = minNumPeers mypaper
                        , rpTimeToDeadline = timeInterval mypaper
                        , rpCurrency       = paperReviewTokenCurrency
                        , rpTokenName      = paperReviewToken
                        }
        cp = CloseParams
                        { ppAuthor         = author mypaper
                        , ppStake          = stake mypaper
                        , ppReward         = reward mypaper
                        , ppNumPeers       = minNumPeers mypaper
                        , ppTimeToDeadline = timeInterval mypaper
                        , ppCurrency       = paperReviewTokenCurrency
                        , ppTokenName      = paperReviewToken
                        , ppFinalManuscript= fileIpns
                        , ppFinalNFTName   = finalNFT
                        }                 

    -- first review
    reviewingProcess h0 h1 (pdlist!!0) ap0 rp scriptAdd $ Map.fromList [("ReviewersLate", False), ("AuthorsLate", False)]
    -- second review
    reviewingProcess h0 h2 (pdlist!!1) ap2 rp scriptAdd $ Map.fromList [("ReviewersLate", False), ("AuthorsLate", False)]
    -- third review
    reviewingProcess h0 h3 (pdlist!!2) ap3 rp scriptAdd $ Map.fromList [("ReviewersLate", False), ("AuthorsLate", False)]

    callEndpoint @"closeAction" h0 cp 
    void $ Emulator.waitNSlots 5
    
    logWalletUtxos "FINAL AUTHOR" (mockWalletAddress w1)
    logWalletUtxos "FINAL SCRIPT" (scriptAdd)
    


-------------------
reviewingProcess ::
    ContractHandle () PaperSchema Text
    -> ContractHandle () PaperSchema Text
    -> [PaperDecision]
    -> AuthorParams
    -> ReviewerParams
    -> Address
    -> Map.Map String Bool
    -> EmulatorTrace ()
reviewingProcess authorWallet reviewerWallet pds aparams rparams addr passedDealine = do
    callEndpoint @"paperSubmission" authorWallet aparams
    void $ Emulator.waitNSlots 9
    loop pds rparams
    callEndpoint @"reviewerAction" reviewerWallet (rparams{rpDecision = last pds})
    void $ Emulator.waitNSlots 5

  where
    loop [] _ = return ()
    loop (pd:pds') revparams = do
        let rparams' = revparams{rpDecision = pd}
        callEndpoint @"reviewerAction" reviewerWallet rparams'
        void $ Emulator.waitNSlots (if Map.lookup "AuthorsLate" passedDealine == Just True then 11 else 10)
        logWalletUtxos "Script" (addr)
        callEndpoint @"authorAction" authorWallet aparams
        void $ Emulator.waitNSlots (if Map.lookup "ReviewersLate" passedDealine == Just True then 11 else 10)
        logWalletUtxos "Author" (mockWalletAddress w1)
        logWalletUtxos "Script" (addr)
        loop pds' rparams'


-----Utils

getUtxos :: Address -> Emulator.EmulatorTrace [(TxOutRef, ChainIndexTxOut)]
getUtxos addr = do
    state <- Emulator.chainState
    let utxoIndex = getIndex $ state ^. Emulator.index 
        utxos     = Data.Maybe.mapMaybe (\(oref, o) -> (oref, ) <$> 
                    fromTxOut o) [(oref, o) | (oref, o) <- Map.toList utxoIndex, txOutAddress o == addr]
    return utxos

logWalletUtxos :: String -> Address -> EmulatorTrace ()
logWalletUtxos role address = do
    finalUtxos <- getUtxos address
    Extras.logInfo $ "Current " ++ role ++ " UTXOS: " ++ show finalUtxos

---- Not used


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
