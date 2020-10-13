{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Db.Schema where

import           Cardano.Db.Schema.Orphans ()
import           Cardano.Db.Types (DbWord64)

import           Data.ByteString.Char8 (ByteString)
import           Data.Int (Int64)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime)
import           Data.WideWord.Word128 (Word128)
import           Data.Word (Word16, Word64)

-- Do not use explicit imports from this module as the imports can change
-- from version to version due to changes to the TH code in Persistent.
import           Database.Persist.TH

import           Shelley.Spec.Ledger.PParams (ProtVer (..))

-- In the schema definition we need to match Haskell types with with the
-- custom type defined in PostgreSQL (via 'DOMAIN' statements). For the
-- time being the Haskell types will be simple Haskell types like
-- 'ByteString' and 'Word64'.

-- We use camelCase here in the Haskell schema definition and 'persistLowerCase'
-- specifies that all the table and column names are converted to lower snake case.

share
  [ mkPersist sqlSettings
  , mkDeleteCascade sqlSettings
  , mkMigrate "migrateCardanoDb"
  ]
  [persistLowerCase|

  -- Schema versioning has three stages to best allow handling of schema migrations.
  --    Stage 1: Set up PostgreSQL data types (using SQL 'DOMAIN' statements).
  --    Stage 2: Persistent generated migrations.
  --    Stage 3: Set up 'VIEW' tables (for use by other languages and applications).
  -- This table should have a single row.
  SchemaVersion
    stageOne Int
    stageTwo Int
    stageThree Int

  PoolHash
    hashRaw             ByteString          sqltype=hash28type
    view                Text
    UniquePoolHash      hashRaw

  SlotLeader
    hash                ByteString          sqltype=hash32type
    poolHashId          PoolHashId Maybe    -- This will be non-null when a block is mined by a pool.
    description         Text                -- Description of the Slots leader.
    UniqueSlotLeader    hash

  -- Each table has autogenerated primary key named 'id', the Haskell type
  -- of which is (for instance for this table) 'BlockId'. This specific
  -- primary key Haskell type can be used in a type-safe way in the rest
  -- of the schema definition.
  -- All NULL-able fields other than 'epochNo' are NULL for EBBs, whereas 'epochNo' is
  -- only NULL for the genesis block.
  Block
    hash                ByteString          sqltype=hash32type
    epochNo             Word64 Maybe        sqltype=uinteger
    slotNo              Word64 Maybe        sqltype=uinteger
    epochSlotNo         Word64 Maybe        sqltype=uinteger
    blockNo             Word64 Maybe        sqltype=uinteger
    previous            BlockId Maybe
    -- Shelley does not have a Merkel Root, but Byron does.
    -- Once we are well into the Shelley era, this column can be dropped.
    merkelRoot          ByteString Maybe    sqltype=hash32type
    slotLeader          SlotLeaderId
    size                Word64              sqltype=uinteger
    time                UTCTime             sqltype=timestamp
    txCount             Word64
    -- Shelley specific
    vrfKey              Text Maybe
    opCert              ByteString Maybe    sqltype=hash32type
    protoVersion        Text Maybe
    UniqueBlock         hash

  Tx
    hash                ByteString          sqltype=hash32type
    block               BlockId                                 -- This type is the primary key for the 'block' table.
    blockIndex          Word64              sqltype=uinteger    -- The index of this transaction within the block.
    outSum              Word64              sqltype=lovelace
    fee                 Word64              sqltype=lovelace
    deposit             Int64                                   -- Needs to allow negaitve values.
    size                Word64              sqltype=uinteger
    UniqueTx            hash

  TxOut
    txId                TxId                -- This type is the primary key for the 'tx' table.
    index               Word16              sqltype=txindex
    address             Text
    addressRaw          ByteString
    paymentCred         ByteString Maybe    sqltype=hash28type
    value               Word64              sqltype=lovelace
    UniqueTxout         txId index          -- The (tx_id, index) pair must be unique.

  TxIn
    txInId              TxId                -- The transaction where this is used as an input.
    txOutId             TxId                -- The transaction where this was created as an output.
    txOutIndex          Word16              sqltype=txindex
    UniqueTxin          txOutId txOutIndex

  -- A table containing metadata about the chain. There will probably only ever be one
  -- row in this table.
  Meta
    startTime           UTCTime             sqltype=timestamp
    networkName         Text
    UniqueMeta          startTime


  -- The following are tables used my specific 'plugins' to the regular cardano-db-sync node
  -- functionality. In the regular cardano-db-sync node these tables will be empty.

  -- The Epoch table is an aggregation of data in the 'Block' table, but is kept in this form
  -- because having it as a 'VIEW' is incredibly slow and inefficient.

  -- The 'outsum' type in the PostgreSQL world is 'bigint >= 0' so it will error out if an
  -- overflow (sum of tx outputs in an epoch) is detected. 'maxBound :: Int` is big enough to
  -- hold 204 times the total Lovelace distribution. The chance of that much being transacted
  -- in a single epoch is relatively low.
  Epoch
    outSum              Word128             sqltype=word128type
    fees                Word64              sqltype=lovelace
    txCount             Word64              sqltype=uinteger
    blkCount            Word64              sqltype=uinteger
    no                  Word64              sqltype=uinteger
    startTime           UTCTime             sqltype=timestamp
    endTime             UTCTime             sqltype=timestamp
    UniqueEpoch         no
    deriving Eq
    deriving Show

  -- -----------------------------------------------------------------------------------------------
  -- Shelley bits

  StakeAddress          -- Can be an address of a script hash
    hashRaw             ByteString          sqltype=addr29type
    view                Text
    registeredTxId      TxId                -- Only used for rollback.
    UniqueStakeAddress  hashRaw

  -- -----------------------------------------------------------------------------------------------
  -- A Pool can have more than one owner, so we have a PoolOwner table that references this one.

  PoolMetaData
    url                 Text
    hash                ByteString          sqltype=hash32type
    registeredTxId      TxId                -- Only used for rollback.
    UniquePoolMetaData  url hash

  PoolUpdate
    hashId              PoolHashId
    certIndex           Word16
    vrfKey              ByteString          sqltype=hash32type
    pledge              DbWord64            sqltype=word64type
    rewardAddrId        StakeAddressId
    activeEpochNo       Word64
    meta                PoolMetaDataId Maybe
    margin              Double                                  -- sqltype=percentage????
    fixedCost           Word64              sqltype=lovelace
    registeredTxId      TxId                                    -- Slot number in which the pool was registered.
    UniquePoolUpdate    hashId registeredTxId

  PoolOwner
    hash                ByteString          sqltype=hash28type
    poolHashId          PoolHashId
    registeredTxId      TxId                                    -- Slot number in which the owner was registered.
    UniquePoolOwner     hash poolHashId registeredTxId

  PoolRetire
    hashId              PoolHashId
    certIndex           Word16
    announcedTxId       TxId                                    -- Slot number in which the pool announced it was retiring.
    retiringEpoch       Word64              sqltype=uinteger    -- Epoch number in which the pool will retire.
    UniquePoolRetiring  hashId announcedTxId

  PoolRelay
    updateId            PoolUpdateId
    ipv4                Text Maybe
    ipv6                Text Maybe
    dnsName             Text Maybe
    dnsSrvName          Text Maybe
    port                Word16 Maybe
    -- Usually NULLables are not allowed in a uniqueness constraint. The semantics of how NULL
    -- interacts with those constraints is non-trivial:  two NULL values are not considered equal
    -- for the purposes of an uniqueness constraint.
    -- Use of "!force" attribute on the end of the line disables this check.
    UniquePoolRelay     updateId ipv4 ipv6 dnsName !force

  -- -----------------------------------------------------------------------------------------------

  Reserve
    addrId              StakeAddressId
    certIndex           Word16
    -- poolId              PoolHashId
    amount              Word64              sqltype=lovelace
    txId                TxId
    UniqueReserves      addrId txId

  Withdrawal
    addrId              StakeAddressId
    -- poolId              PoolHashId
    amount              Word64              sqltype=lovelace
    txId                TxId
    UniqueWithdrawal    addrId txId

  Delegation
    addrId              StakeAddressId
    certIndex           Word16
    poolHashId          PoolHashId
    activeEpochNo       Word64
    txId                TxId
    UniqueDelegation    addrId poolHashId txId

  -- When was a staking key/script registered
  StakeRegistration
    addrId              StakeAddressId
    certIndex           Word16
    txId                TxId
    UniqueStakeRegistration addrId txId

  -- When was a staking key/script deregistered
  StakeDeregistration
    addrId              StakeAddressId
    certIndex           Word16
    txId                TxId
    UniqueStakeDeregistration addrId txId

  TxMetadata
    key                 DbWord64            sqltype=word64type
    json                Text                sqltype=jsonb
    txId                TxId
    UniqueTxMetadata    key txId

  -- -----------------------------------------------------------------------------------------------
  -- Reward, Stake and Treasury need to be obtained from the ledger state.

    -- The reward for each stake address and. This is not a balance, but a reward amount and the
    -- epoch in which the reward was earned.
  Reward
    addrId              StakeAddressId
    amount              Word64              sqltype=lovelace
    epochNo             Word64
    poolId              PoolHashId
    blockId             BlockId
    UniqueReward        addrId blockId

  EpochStake
    addrId              StakeAddressId
    poolId              PoolHashId
    amount              Word64              sqltype=lovelace
    epochNo             Word64
    blockId             BlockId             -- To make rollbacks work correctly.
    UniqueStake         addrId epochNo

  Treasury
    addrId              StakeAddressId
    certIndex           Word16
    -- poolId              PoolHashId
    amount              Word64              sqltype=lovelace
    txId                TxId
    UniqueTreasury      addrId txId

  -- -----------------------------------------------------------------------------------------------
  -- Update parameter proposals.

  ParamProposal
    epochNo             Word64              sqltype=uinteger
    key                 ByteString          sqltype=hash28type
    minFeeA             Word64 Maybe        sqltype=uinteger
    minFeeB             Word64 Maybe        sqltype=uinteger
    maxBlockSize        Word64 Maybe        sqltype=uinteger
    maxTxSize           Word64 Maybe        sqltype=uinteger
    maxBhSize           Word64 Maybe        sqltype=uinteger
    keyDeposit          Word64 Maybe        sqltype=lovelace
    poolDeposit         Word64 Maybe        sqltype=lovelace
    maxEpoch            Word64 Maybe        sqltype=uinteger
    optimalPoolCount    Word64 Maybe        sqltype=uinteger
    influence           Double Maybe        -- sqltype=rational
    monetaryExpandRate  Double Maybe        -- sqltype=interval
    treasuryGrowthRate  Double Maybe        -- sqltype=interval
    decentralisation    Double Maybe        -- sqltype=interval
    entropy             ByteString Maybe    sqltype=hash32type
    protocolVersion     ProtVer Maybe
    minUtxoValue        Word64 Maybe        sqltype=lovelace
    minPoolCost         Word64 Maybe        sqltype=lovelace

    registeredTxId      TxId                -- Slot number in which update registered.
    UniqueParamProposal key registeredTxId

  EpochParam
    epochNo             Word64              sqltype=uinteger
    minFeeA             Word64              sqltype=uinteger
    minFeeB             Word64              sqltype=uinteger
    maxBlockSize        Word64              sqltype=uinteger
    maxTxSize           Word64              sqltype=uinteger
    maxBhSize           Word64              sqltype=uinteger
    keyDeposit          Word64              sqltype=lovelace
    poolDeposit         Word64              sqltype=lovelace
    maxEpoch            Word64              sqltype=uinteger
    optimalPoolCount    Word64              sqltype=uinteger
    influence           Double              -- sqltype=rational
    monetaryExpandRate  Double              -- sqltype=interval
    treasuryGrowthRate  Double              -- sqltype=interval
    decentralisation    Double              -- sqltype=interval
    entropy             ByteString Maybe    sqltype=hash32type
    protocolVersion     ProtVer
    minUtxoValue        Word64              sqltype=lovelace
    minPoolCost         Word64              sqltype=lovelace

    blockId             BlockId             -- The first block where these parameters are valid.
    UniqueEpochParam    epochNo blockId

  |]
