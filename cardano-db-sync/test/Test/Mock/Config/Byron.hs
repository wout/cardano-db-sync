{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Mock.Config.Byron
  ( mkByronConfig
  ) where

import           Cardano.Chain.Common (Address, BlockCount (..), CompactAddress, KeyHash, Lovelace,
                   TxFeePolicy (..), TxSizeLinear (..), mkLovelace, rationalToLovelacePortion)
import           Cardano.Chain.Delegation (Certificate)
import           Cardano.Chain.Genesis (Config (..), GenesisAvvmBalances (..), GenesisData (..),
                   GenesisDelegation (..), GenesisKeyHashes (..), GenesisNonAvvmBalances (..))
import           Cardano.Chain.Slotting (EpochNumber (..), SlotNumber (..))
import           Cardano.Chain.UTxO (UTxOConfiguration (..))
import           Cardano.Chain.Update (ProtocolParameters (..), SoftforkRule (..))
import           Cardano.Crypto.ProtocolMagic (ProtocolMagicId (..),
                   RequiresNetworkMagic (RequiresNoMagic))
import           Cardano.Crypto.Signing.Redeem (CompactRedeemVerificationKey)

import           Cardano.Prelude

import           Data.Time.Clock ()

import           Prelude (read)

mkByronConfig :: Word32 -> Config
mkByronConfig magicId =
  Config
    { configGenesisData = mkGenesisData magicId
    , configGenesisHash = panic "Test.Mock.Config.Byron: Byron GenesisHash" -- This is probably not checked
    , configReqNetMagic = RequiresNoMagic -- Same as mainnet
    , configUTxOConfiguration = uTxOConfiguration
    }

-- -------------------------------------------------------------------------------------------------

genesisAvvmBalance :: GenesisAvvmBalances
genesisAvvmBalance =
  GenesisAvvmBalances
    { unGenesisAvvmBalances = mempty :: Map CompactRedeemVerificationKey Lovelace
    }

genesisDelegation :: GenesisDelegation
genesisDelegation =
  UnsafeGenesisDelegation
    { unGenesisDelegation = mempty :: Map KeyHash Certificate
    }

genesisKeyHashes :: GenesisKeyHashes
genesisKeyHashes =
  GenesisKeyHashes
    { unGenesisKeyHashes = mempty :: Set KeyHash
    }

genesisNonAvvmBalances :: GenesisNonAvvmBalances
genesisNonAvvmBalances =
  GenesisNonAvvmBalances
    { unGenesisNonAvvmBalances = mempty :: Map Address Lovelace
    }

mkGenesisData :: Word32 -> GenesisData
mkGenesisData magicId =
  GenesisData
    { gdGenesisKeyHashes = genesisKeyHashes
    , gdHeavyDelegation = genesisDelegation
    , gdStartTime = read "2021-10-01 00:00:00.0 UTC"
    , gdNonAvvmBalances = genesisNonAvvmBalances
    , gdProtocolParameters = protocolParameters
    , gdK = BlockCount 10 -- Security parameter
    , gdProtocolMagicId = ProtocolMagicId magicId
    , gdAvvmDistr = genesisAvvmBalance
    }

protocolParameters :: ProtocolParameters
protocolParameters =
  ProtocolParameters
    { ppScriptVersion = 0 -- :: !Word16
    , ppSlotDuration = 20000 -- milliseconds
    , ppMaxBlockSize = 0 -- :: !Natural
    , ppMaxHeaderSize = 0 -- :: !Natural
    , ppMaxTxSize = 0 -- :: !Natural
    , ppMaxProposalSize = 0 -- :: !Natural
    , ppMpcThd = rationalToLovelacePortion 1
    , ppHeavyDelThd = rationalToLovelacePortion 1
    , ppUpdateVoteThd = rationalToLovelacePortion 1
    , ppUpdateProposalThd = rationalToLovelacePortion 1

    -- | Time to live for a protocol update proposal. This used to be the number
    -- of slots after which the system made a decision regarding an update
    -- proposal confirmation, when a majority of votes was not reached in the
    -- given number of slots. If there were more positive than negative votes the
    -- proposal became confirmed, otherwise it was rejected. Since in the
    -- Byron-Shelley bridge we do not have negative votes, and we aim at
    -- simplifying the update mechanism, 'ppUpdateProposalTTL' is re-interpreted as
    -- the number of slots a proposal has to gather a majority of votes. If a
    -- majority of votes has not been reached before this period, then the
    -- proposal is rejected.
    --
    -- -- TODO: it seems this should be a slot count.
    , ppUpdateProposalTTL = SlotNumber 10
    , ppSoftforkRule = softforkRule
    , ppTxFeePolicy = TxFeePolicyTxSizeLinear txSizeLinear
    , ppUnlockStakeEpoch = EpochNumber 10000
    }

softforkRule :: SoftforkRule
softforkRule =
  SoftforkRule
    { srInitThd = rationalToLovelacePortion 1
    , srMinThd = rationalToLovelacePortion 1
    , srThdDecrement = rationalToLovelacePortion 1
    }

txSizeLinear :: TxSizeLinear
txSizeLinear =
  TxSizeLinear
    (fromRight (panic "Test.Mock.Config.Byron.txSizeLinear") $ mkLovelace 1)
    1

uTxOConfiguration :: UTxOConfiguration
uTxOConfiguration =
  UTxOConfiguration
    { tcAssetLockedSrcAddrs = mempty :: Set CompactAddress
    }
