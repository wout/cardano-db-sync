{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
module Cardano.Sync.Types
  ( Block (..)
  , BlockDetails (..)
  , CardanoBlock
  , CardanoPoint
  , CardanoProtocol
  , EpochSlot (..)
  , FetchResult (..)
  , PoolFetchRetry (..)
  , PoolKeyHash
  , PoolMetaHash (..)
  , PoolMetaHex (..)
  , MetricSetters (..)
  , Retry (..)
  , SlotDetails (..)
  , poolMetahashToHex
  , poolMetaHexToHash
  ) where

import           Cardano.Prelude hiding (Meta)

import qualified Cardano.Ledger.Keys as Ledger

import           Cardano.Db (PoolHashId, PoolMetadataRefId, PoolOfflineData, PoolOfflineFetchError,
                   PoolUrl)

import           Cardano.Sync.Config.Types (CardanoBlock, CardanoProtocol)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..), SlotNo (..))

import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (POSIXTime)

import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import           Ouroboros.Network.Block (BlockNo, Point)

import           Quiet (Quiet (..))


type CardanoPoint = Point CardanoBlock

type PoolKeyHash = Ledger.KeyHash 'Ledger.StakePool StandardCrypto

data BlockDetails = BlockDetails
  { bdBlock :: !CardanoBlock
  , bdSlot :: !SlotDetails
  }

-- | Slot within an Epoch.
newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Ord, Show)

data FetchResult
    = ResultMetadata !PoolOfflineData
    | ResultError !PoolOfflineFetchError
    deriving Show

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

-- The hash must be unique!
data Block = Block
  { bHash :: !ByteString
  , bEpochNo :: !EpochNo
  , bSlotNo  :: !SlotNo
  , bBlockNo :: !BlockNo
  } deriving (Eq, Show)

-- The metrics we use.
-- Kept as a separate struct and do not put into environment because
-- when we need to test functions using this we need to initialize the
-- whole environment and not just pass in the layer. This shows clearly
-- that it needs to remain a separate parameter passed around where needed.
data MetricSetters = MetricSetters
  { metricsSetNodeBlockHeight :: BlockNo -> IO ()
  , metricsSetDbQueueLength :: Natural -> IO ()
  , metricsSetDbBlockHeight :: BlockNo -> IO ()
  , metricsSetDbSlotHeight :: SlotNo -> IO ()
  }

data PoolFetchRetry = PoolFetchRetry
  { pfrPoolHashId :: !PoolHashId
  , pfrReferenceId :: !PoolMetadataRefId
  , pfrPoolUrl :: !PoolUrl
  , pfrPoolMDHash :: !PoolMetaHash
  , pfrRetry :: !Retry
  } deriving (Show)


data Retry = Retry
  { retryFetchTime :: !POSIXTime -- Time last time time
  , retryRetryTime :: !POSIXTime -- Time to retry
  , retryCount :: !Word
  } deriving (Eq, Show, Generic)




newtype PoolMetaHash
  = PoolMetaHash { unPoolMetaHash :: ByteString }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetaHash)

newtype PoolMetaHex
  = PoolMetaHex { unPoolMetaHex :: Text }
  deriving (Eq, Ord, Generic)
  deriving Show via (Quiet PoolMetaHex)

poolMetahashToHex :: PoolMetaHash -> PoolMetaHex
poolMetahashToHex (PoolMetaHash hash) = PoolMetaHex $ Text.decodeLatin1 (Base16.encode hash)

poolMetaHexToHash :: PoolMetaHex -> Either String PoolMetaHash
poolMetaHexToHash (PoolMetaHex hash) = PoolMetaHash <$> Base16.decode (Text.encodeUtf8 hash)
