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
 
module TestsParallel where

import           Tests (logWalletUtxos)
import           Types
import           TxConstruction
import           ReviewContract   
import           Control.Monad              hiding (fmap)
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot()
import           Ledger.Value 
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, div)
import           Wallet.Emulator.Wallet


paperReviewTokenCurrency :: CurrencySymbol
paperReviewTokenCurrency = "2023"

paperReviewToken :: TokenName
paperReviewToken = "PaperReviewToken"


peerIdTokenCurrency :: CurrencySymbol
peerIdTokenCurrency = "3023"

peerIdToken01, peerIdToken02, peerIdToken03 :: TokenName
peerIdToken01 = "PeerIdentityToken01"
peerIdToken02 = "PeerIdentityToken02"
peerIdToken03 = "PeerIdentityToken03"

w1, w2, w3 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3

test :: IO ()
test = runEmulatorTraceIO' def emCfg $ myTrace'
  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue (AssetClass (paperReviewTokenCurrency, paperReviewToken)) 4)
                    , (w2, v <> assetClassValue (AssetClass (peerIdTokenCurrency, peerIdToken01)) 1)
                    , (w3, v <> assetClassValue (AssetClass (peerIdTokenCurrency, peerIdToken02)) 1)
                    ]
                }
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000

---------------------------

myTrace' :: EmulatorTrace ()
myTrace'  = do
   
    h0 <- activateContractWallet w1 endpoints
    h1 <- activateContractWallet w2 endpoints
    h2 <- activateContractWallet w3 endpoints

    let mypaper = Paper { 
      author           = mockWalletPaymentPubKeyHash w1
    , stake            = 100_000_000
    , compensation     = stake mypaper `div` 2
    , minNumPeers      = 2
    , timeInterval     = POSIXTime {getPOSIXTime = 10_000}
    , paperToken       = (AssetClass (paperReviewTokenCurrency, paperReviewToken))
    } 

    let pkh_reviewer1   = mockWalletPaymentPubKeyHash w2
        pkh_reviewer2   = mockWalletPaymentPubKeyHash w3
        fileIpns  = Manuscript "/ipns/QmS4ust...4uVv"
        finalNFT  = "PeerReviewedNFT./ipns/QmS4ust...4uVv"
        scriptAdd = paperAddress mypaper      
        ap0 = AuthorParams 
                        { upReviewer        = Just pkh_reviewer1
                        , upStake           = stake mypaper
                        , upReward          = compensation mypaper
                        , upNumPeers        = minNumPeers mypaper
                        , upTimeToDeadline  = timeInterval mypaper
                        , upCurrency        = paperReviewTokenCurrency
                        , upTokenName       = paperReviewToken
                        , upManuscript      = fileIpns
                        }
        ap2 = ap0{upReviewer = Just pkh_reviewer2}
        rp = ReviewerParams
                        { rpAuthor         = author mypaper
                        , rpStake          = stake mypaper
                        , rpReward         = compensation mypaper
                        , rpNumPeers       = minNumPeers mypaper
                        , rpTimeToDeadline = timeInterval mypaper
                        , rpCurrency       = paperReviewTokenCurrency
                        , rpTokenName      = paperReviewToken
                        }
        
        cp = CloseParams
                        { ppAuthor         = author mypaper
                        , ppStake          = stake mypaper
                        , ppReward         = compensation mypaper
                        , ppNumPeers       = minNumPeers mypaper
                        , ppTimeToDeadline = timeInterval mypaper
                        , ppCurrency       = paperReviewTokenCurrency
                        , ppTokenName      = paperReviewToken
                        , ppFinalManuscript= fileIpns
                        , ppFinalNFTName   = finalNFT
                        }    


    callEndpoint @"paperSubmission" h0 ap0 -- X
    void $ Emulator.waitNSlots 3   

    callEndpoint @"paperSubmission" h0 ap2 -- Y
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h1 rp{rpDecision = Minor} -- X
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h2 rp{rpDecision = Major} -- Y
    void $ Emulator.waitNSlots 3

    callEndpoint @"authorAction" h0 ap0 -- X
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h1 rp{rpDecision = Accept} -- X
    void $ Emulator.waitNSlots 3

    callEndpoint @"authorAction" h0 ap2 -- Y
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h2 rp{rpDecision = Accept} -- Y
    void $ Emulator.waitNSlots 3
{-
    callEndpoint @"closeAction" h0 cp 
    void $ Emulator.waitNSlots 3
-}
    callEndpoint @"authorAction" h0 ap0 -- X 
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h1 rp{rpDecision = Accept} -- X
    void $ Emulator.waitNSlots 3

    callEndpoint @"authorAction" h0 ap2 -- Y
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h2 rp{rpDecision = Accept} -- Y
    void $ Emulator.waitNSlots 3 

    callEndpoint @"closeAction" h0 cp 
    void $ Emulator.waitNSlots 5   

    logWalletUtxos "FINAL AUTHOR" (mockWalletAddress w1)
    logWalletUtxos "FINAL SCRIPT" (scriptAdd)
    
