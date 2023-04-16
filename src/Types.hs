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
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Types where

import qualified PlutusTx
import           Ledger                 hiding (singleton)
import qualified PlutusTx.Builtins      as Builtins
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Playground.Contract    (ToSchema)
import qualified Prelude                as Haskell (Eq, Ord, Show, Integer)
import           Data.Aeson             (FromJSON, ToJSON)
import           GHC.Generics           (Generic)  
import qualified Data.Map               as Map

------------------ Script Parameter --------------------------------------------------------------------------------------

data Paper = Paper
    { author           :: PaymentPubKeyHash
    , stake            :: Integer
    , compensation     :: Integer
    , minNumPeers      :: Integer
    , timeInterval     :: POSIXTime 
    , paperToken       :: AssetClass
    } deriving (Haskell.Show, Generic, FromJSON, ToJSON, Haskell.Eq, Haskell.Ord)
PlutusTx.makeLift ''Paper

------------------ Helper types --------------------------------------------------------------------------------------

newtype Round = Round Haskell.Integer
    deriving (Haskell.Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq)
instance Eq Round where    
    {-# INLINABLE (==) #-}    
    Round n == Round n'  = n == n'
    _       == _         = False 
PlutusTx.unstableMakeIsData ''Round


newtype Manuscript = Manuscript BuiltinByteString
    deriving (Haskell.Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq)
instance Eq Manuscript where  
    {-# INLINABLE (==) #-}    
    Manuscript bs == Manuscript bs' = bs == bs'
PlutusTx.unstableMakeIsData ''Manuscript

------------------ Reviewer's Decision --------------------------------------------------------------------------------------

data PaperDecision = Accept | Minor | Major | Reject 
    deriving (Haskell.Show, Generic, FromJSON, ToJSON, ToSchema, Haskell.Eq)
instance Eq PaperDecision where
    {-# INLINABLE (==) #-}
    Accept == Accept = True
    Minor  == Minor  = True
    Major  == Major  = True
    Reject == Reject = True
    _      == _      = False
PlutusTx.unstableMakeIsData ''PaperDecision


------------------ Contract State --------------------------------------------------------------------------------------

data PaperStatus = Submitted Round | Reviewed Round | Closed Round
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

instance Eq PaperStatus where
    {-# INLINABLE (==) #-}   
    (Submitted r) == (Submitted r') = (r == r')
    (Reviewed r) == (Reviewed r') = (r == r')
    (Closed r) == (Closed r') = (r == r')
    _ == _ = False
PlutusTx.unstableMakeIsData ''PaperStatus


------------------ Script Datum --------------------------------------------------------------------------------------

data PaperDatum =
    PaperDatum
    {
        d_linkToManuscript   :: Manuscript, 
        d_reviewerPkh        :: Maybe PaymentPubKeyHash, 
        d_currentDecision    :: Maybe PaperDecision, 
        d_nextDeadline       :: Maybe POSIXTime, 
        d_status             :: PaperStatus,
        d_allRevDecisions    :: Maybe [(PaymentPubKeyHash, PaperDecision)], 
        d_peerReviewed       :: Bool
    }
    deriving (Haskell.Show, Generic, FromJSON, ToJSON, Haskell.Eq)
instance Eq PaperDatum where
    {-# INLINABLE (==) #-}
    a == b    = (d_linkToManuscript a == d_linkToManuscript b)
                && (d_reviewerPkh a == d_reviewerPkh b)
                && (d_currentDecision a == d_currentDecision b)
                && (d_nextDeadline a == d_nextDeadline b)
                && (d_status a == d_status b)
                && (d_allRevDecisions a == d_allRevDecisions b)
                && (d_peerReviewed a == d_peerReviewed b)
PlutusTx.makeIsDataIndexed ''PaperDatum [('PaperDatum,0)]


------------------ Script Redeemer --------------------------------------------------------------------------------------

data PaperRedeemer =   Revision PaperDecision 
                    | UpdatedAt Manuscript
                    | ClosedAt Manuscript
                    | ClaimAuthor 
                    | ClaimReviewer 
                    | PeerReviewed Manuscript
    deriving Haskell.Show
PlutusTx.unstableMakeIsData ''PaperRedeemer













