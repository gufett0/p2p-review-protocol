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

import           Tests (getUtxos, logWalletUtxos)
import           Types
import           TxConstruction
import           ReviewContract         
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot()
import           Ledger.Value 
import           Ledger.Ada                 as Ada
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..), String, div, last)
import           Wallet.Emulator.Wallet



paperReviewTokenCurrency :: CurrencySymbol
paperReviewTokenCurrency = "2023"

paperReviewToken :: TokenName
paperReviewToken = "PaperReviewToken"

w1, w2, w3 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3

test' :: IO ()
test' = runEmulatorTraceIO' def emCfg $ myTrace'
  where
    emCfg :: EmulatorConfig
    emCfg = def { _initialChainState = Left $ Map.fromList
                    [ (w1, v <> assetClassValue (AssetClass (paperReviewTokenCurrency, paperReviewToken)) 4)
                    , (w2, v)
                    , (w3, v)
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
    , reward           = stake mypaper `div` 2
    , timeInterval     = POSIXTime {getPOSIXTime = 10_000}
    , paperNFT         = (AssetClass (paperReviewTokenCurrency, paperReviewToken))
    } 

    let pkh_reviewer1   = mockWalletPaymentPubKeyHash w2
        pkh_reviewer2   = mockWalletPaymentPubKeyHash w3
        fileIpns  = Manuscript "ipns_address"
        scriptAdd = paperAddress mypaper      
        ap0 = AuthorParams 
                        { upReviewer        = pkh_reviewer1
                        , upStake           = stake mypaper
                        , upReward          = reward mypaper
                        , upTimeToDeadline  = timeInterval mypaper
                        , upCurrency        = paperReviewTokenCurrency
                        , upTokenName       = paperReviewToken
                        , upManuscript      = fileIpns
                        }
        ap2 = ap0{upReviewer = pkh_reviewer2}
        rp = ReviewerParams
                        { rpAuthor         = author mypaper
                        , rpStake          = stake mypaper
                        , rpReward         = reward mypaper
                        , rpTimeToDeadline = timeInterval mypaper
                        , rpCurrency       = paperReviewTokenCurrency
                        , rpTokenName      = paperReviewToken
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

    callEndpoint @"reviewerAction" h2 rp{rpDecision = Reject} -- Y
    void $ Emulator.waitNSlots 3

    callEndpoint @"authorAction" h0 ap0 -- X 
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h1 rp{rpDecision = Accept} -- X
    void $ Emulator.waitNSlots 3

    callEndpoint @"authorAction" h0 ap2 -- Y
    void $ Emulator.waitNSlots 3

    callEndpoint @"reviewerAction" h2 rp{rpDecision = Reject} -- Y
    void $ Emulator.waitNSlots 3

    logWalletUtxos "Script" (scriptAdd)
