{-# LANGUAGE DataKinds #-}
module Cardano.Sync.Types
  ( BlockDetails (..)
  , CardanoBlock
  , CardanoPoint
  , CardanoProtocol
  , EpochSlot (..)
  , SlotDetails (..)
  , SyncState (..)
  , Block (..)
  , Meta (..)
  , MetricsLayer (..)
  ) where

import           Cardano.Prelude hiding (Meta)

import           Cardano.Sync.Config.Types (CardanoBlock, CardanoProtocol)

import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Data.Time.Clock (UTCTime)

import           Ouroboros.Network.Block (Point)

type CardanoPoint = Point CardanoBlock

data BlockDetails = BlockDetails
  { bdBlock :: !CardanoBlock
  , bdSlot :: !SlotDetails
  }

newtype EpochSlot = EpochSlot
  { unEpochSlot :: Word64
  } deriving (Eq, Show)

data SlotDetails = SlotDetails
  { sdSlotTime :: !UTCTime
  , sdCurrentTime :: !UTCTime
  , sdEpochNo :: !EpochNo
  , sdEpochSlot :: !EpochSlot
  , sdEpochSize :: !EpochSize
  } deriving (Eq, Show)

data SyncState
  = SyncLagging         -- Local tip is lagging the global chain tip.
  | SyncFollowing       -- Local tip is following global chain tip.
  deriving (Eq, Show)

-- The hash must be unique!
data Block = Block
    { bHash    :: !ByteString
    , bEpochNo :: !(Maybe Word64)
    , bSlotNo  :: !(Maybe Word64)
    , bBlockNo :: !(Maybe Word64)
    } deriving (Eq, Show)

-- The startTime must be unique!
data Meta = Meta
    { mStartTime     :: !UTCTime
    , mNetworkName   :: !(Maybe Text)
    } deriving (Eq, Show)

-- @Word64@ is valid as well.
newtype BlockId = BlockId Int
    deriving (Eq, Show)

-- @Word64@ is valid as well.
newtype MetaId = MetaId Int
    deriving (Eq, Show)

-- The metrics we use.
-- Kept as a separate struct and do not put into environment because
-- when we need to test functions using this we need to initialize the
-- whole environment and not just pass in the layer. This shows clearly
-- that it needs to remain a separate parameter passed around where needed.
data MetricsLayer = MetricsLayer
    { mlSetNodeBlockHeight :: Word64 -> IO ()
    , mlSetDbQueueLength :: Natural -> IO ()
    , mlSetDbBlockHeight :: Word64 -> IO ()
    , mlSetDbSlotHeight :: Word64 -> IO ()
    }

