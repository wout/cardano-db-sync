{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Sync.Api
  ( SyncEnv (..)
  , LedgerEnv (..)
  , SyncDataLayer (..)
  , mkSyncEnvFromConfig
  , verifyFilePoints
  , getLatestPoints
  ) where

import           Cardano.Prelude (Proxy (..), catMaybes, find)

import           Cardano.BM.Trace (Trace)


import qualified Cardano.Chain.Genesis as Byron
import           Cardano.Crypto.ProtocolMagic

import           Cardano.Db (textShow)

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Shelley.Genesis as Shelley

import           Cardano.Slotting.Slot (SlotNo (..))

import           Cardano.Sync.Config.Cardano
import           Cardano.Sync.Config.Shelley
import           Cardano.Sync.Config.Types
import           Cardano.Sync.Error
import           Cardano.Sync.LedgerState
import           Cardano.Sync.Types

import           Data.ByteString (ByteString)
import           Data.Text (Text)

import           Ouroboros.Consensus.Block.Abstract (HeaderHash, fromRawHash)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo)
import           Ouroboros.Network.Block (Point (..))
import           Ouroboros.Network.Magic (NetworkMagic (..))
import qualified Ouroboros.Network.Point as Point


data SyncEnv = SyncEnv
  { envProtocol :: !SyncProtocol
  , envNetworkMagic :: !NetworkMagic
  , envSystemStart :: !SystemStart
  , envDataLayer :: !SyncDataLayer
  , envLedger :: !LedgerEnv
  }

-- The base @DataLayer@ that contains the functions required for syncing to work.
data SyncDataLayer = SyncDataLayer
  { sdlGetSlotHash :: SlotNo -> IO [(SlotNo, ByteString)]
  , sdlGetLatestBlock :: IO (Maybe Block)
  , sdlGetLatestSlotNo :: IO SlotNo
  }

mkSyncEnv
    :: SyncDataLayer -> Trace IO Text -> ProtocolInfo IO CardanoBlock -> Ledger.Network
    -> NetworkMagic -> SystemStart -> LedgerStateDir -> EpochSlot -> EpochSlot
    -> IO SyncEnv
mkSyncEnv dataLayer trce protoInfo nw nwMagic systemStart dir stableEpochSlot rewardsDoneEpochSlot = do
  ledgerEnv <- mkLedgerEnv trce protoInfo dir nw stableEpochSlot rewardsDoneEpochSlot
  pure $ SyncEnv
          { envProtocol = SyncProtocolCardano
          , envNetworkMagic = nwMagic
          , envSystemStart = systemStart
          , envDataLayer = dataLayer
          , envLedger = ledgerEnv
          }

mkSyncEnvFromConfig :: SyncDataLayer -> Trace IO Text -> LedgerStateDir -> GenesisConfig -> IO (Either SyncNodeError SyncEnv)
mkSyncEnvFromConfig trce dataLayer dir genCfg =
    case genCfg of
      GenesisCardano _ bCfg sCfg _aCfg
        | unProtocolMagicId (Byron.configProtocolMagicId bCfg) /= Shelley.sgNetworkMagic (scConfig sCfg) ->
            pure . Left . NECardanoConfig $
              mconcat
                [ "ProtocolMagicId ", textShow (unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                , " /= ", textShow (Shelley.sgNetworkMagic $ scConfig sCfg)
                ]
        | Byron.gdStartTime (Byron.configGenesisData bCfg) /= Shelley.sgSystemStart (scConfig sCfg) ->
            pure . Left . NECardanoConfig $
              mconcat
                [ "SystemStart ", textShow (Byron.gdStartTime $ Byron.configGenesisData bCfg)
                , " /= ", textShow (Shelley.sgSystemStart $ scConfig sCfg)
                ]
        | otherwise ->
            Right <$> mkSyncEnv trce dataLayer (mkProtocolInfoCardano genCfg) (Shelley.sgNetworkId $ scConfig sCfg)
                        (NetworkMagic . unProtocolMagicId $ Byron.configProtocolMagicId bCfg)
                        (SystemStart .Byron.gdStartTime $ Byron.configGenesisData bCfg)
                        dir (calculateStableEpochSlot $ scConfig sCfg)
                        (calculateRewardsDoneEpochSlot $ scConfig sCfg)


getLatestPoints :: SyncEnv -> IO [CardanoPoint]
getLatestPoints env = do
    files <- listLedgerStateFilesOrdered $ leDir (envLedger env)
    verifyFilePoints env files

verifyFilePoints :: SyncEnv -> [LedgerStateFile] -> IO [CardanoPoint]
verifyFilePoints env files =
    catMaybes <$> mapM validLedgerFileToPoint files
  where
    validLedgerFileToPoint :: LedgerStateFile -> IO (Maybe CardanoPoint)
    validLedgerFileToPoint lsf = do
        hashes <- sdlGetSlotHash (envDataLayer env) (lsfSlotNo lsf)
        let valid  = find (\(_, h) -> lsfHash lsf == hashToAnnotation h) hashes
        case valid of
          Just (slot, hash) | slot == lsfSlotNo lsf -> pure $ convert (slot, hash)
          _ -> pure Nothing

    convert :: (SlotNo, ByteString) -> Maybe CardanoPoint
    convert (slot, hashBlob) =
      Point . Point.block slot <$> convertHashBlob hashBlob

    convertHashBlob :: ByteString -> Maybe (HeaderHash CardanoBlock)
    convertHashBlob = Just . fromRawHash (Proxy @CardanoBlock)

-- -------------------------------------------------------------------------------------------------
-- This is incredibly suboptimal. It should work, for now, but may break at some future time and
-- when it is wrong then data in `db-sync` will simply be wrong and we do not have any way of
-- detecting that it is wrong.
--
-- An epoch is `10 k / f` long, and the stability window is `3 k / f` so the time from the start
-- of the epoch to start of the stability window is `7 k / f`.
--
-- Hopefully lower level libraries will be able to provide us with something better than this soon.
calculateStableEpochSlot :: Shelley.ShelleyGenesis era -> EpochSlot
calculateStableEpochSlot cfg = EpochSlot $ ceiling (7.0 * deciEpoch cfg)

-- The rewards should have been calculayed by this time. If they are not, the rewards table may
-- not be complete.
calculateRewardsDoneEpochSlot :: Shelley.ShelleyGenesis era -> EpochSlot
calculateRewardsDoneEpochSlot cfg = EpochSlot $ ceiling (8.0 * deciEpoch cfg)

-- One deci-epoch is 1/10 of an epoch.
-- One deci-epoch is k / f.
deciEpoch :: Shelley.ShelleyGenesis era -> Double
deciEpoch cfg =
    secParam / actSlotCoeff
  where
    secParam :: Double
    secParam = fromIntegral $ Shelley.sgSecurityParam cfg

    actSlotCoeff :: Double
    actSlotCoeff = fromRational (Ledger.unboundRational $ Shelley.sgActiveSlotsCoeff cfg)
