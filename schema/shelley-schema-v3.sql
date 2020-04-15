------------------------------------------------------------
--
-- Schema to store blocks for the Shelley era of the Cardano chain in a PostgreSQL database.
-- This schema is based on that for the Byron era, but for Shelley era blocks only.
--
-- Kevin Hammond, IOHK
--       2020-04-10   KH        Adapted Byron version
--       2020-04-13   KH        Created Shelley-only version
--       2020-04-14   KH        Updated types and included transaction metadata
--       2020-04-14   KH        Worked through with Jared, checked sizes and made minor updates
--       2020-04-15   KH        Updated metadata format, updated address format, added epoch no in block
--
------------------------------------------------------------



------------------------------------------------------------
-- Key open issues
--
--   1.  Do we need historic data for pool registrations etc. or just live information?
--       (Yes)
--   2.  Are withdrawals needed in the transactions?
--       (Yes.  We need this to calculate wallet reward totals from the on-chain data.)
--   3.  Are there other forms of transaction metadata that need to be recorded?
--       (Yes - to do add these.)
--   4.  Do we need to record protocol parameter changes
--       (Yes. will need to follow the voting procedure to determine this, this could be for later implementation)
--   5.  Can script and key hashes be treated identically (address hashes are a different size)
--       (No.  script hash is an address, stake key hash is an address, pool operator key hash no - to do: ask Jared to check each of these)
--       (don't need to store scripts, since not executed, only used for authorisation)
--       (do we need to distinguish script hash from key hash - possibly, since there could in theory be a collision?)
--   6.  Do we need to record pointer addresses as an additional type, or are they just internal to the ledger?
--       (address type will change, but could have (payment credential [hash],staking reference[NULL,hash,pointer - [block,tx,cert] triple])
--   7.  Are signatures variable sized?
--       (probably not, but needs to be confirmed - just wrapped up cryptonite addresses)
--   8.  Are there additional constraints on data formats that should be included in the schema (e.g. data ranges)
--       (To do: check all constraints)
--   9.  Are alternatives handled correctly?  Is there a better way to do this in PostgreSQL than just using tables
--       (Checking this.  Looks like no.)
--   10. Can we run a central node to access treasury/reward info rather than reconstructing this information
--       (No. We will need to record all the data we need in a local node.  Beware of performance implications in the implementation.)
--   11. Do we need to merge with the existing Byron schema or can we maintain two schemas
--       (Merging is preferred.  To do: do this and normalise the data format once the necessary data content is agreed.)
--
------------------------------------------------------------



----------------------------------------
--
-- Basic Type Definitions
--
----------------------------------------

-- Coin values in Lovelace.
CREATE DOMAIN lovelace AS bigint
  CHECK (VALUE >= 0 AND VALUE <= 45000000000000000);

-- Unsigned (non-negative) integers, used in many places.
CREATE DOMAIN uinteger AS integer
  CHECK (VALUE >= 0);

-- Positive integers.
CREATE DOMAIN natural AS integer
  CHECK (VALUE > 0);

-- Copied from the CDDL spec.  interval may not be the best name?
CREATE TYPE interval (
   min   uinteger       NOT NULL,
   max   uinteger       NOT NULL
   );

CREATE TYPE rational (
   num      uinteger    NOT NULL,
   denom    natural     NOT NULL
   );

-- To do: check this is correct syntax
CREATE DOMAIN nonce as hash;


----------------------------------------
--
-- Address and Key Hashes
--
----------------------------------------


-- Many entities are identified by hash. Most hashes are 256bit, 32bytes.
CREATE DOMAIN hash AS bytea
  CHECK (octet_length (VALUE) = 32);


-- Address hashes are, however, 224bit, 28bytes.
CREATE DOMAIN addr_hash AS bytea
  CHECK (octet_length (VALUE) = 28);


-- NEW for SHELLEY
-- Used to separate address hashes for semantic reasons
-- The hashes are credentials, which may be either key hashes or script hashes
-- To do: Are these hashes or addr_hashes?

CREATE DOMAIN payment_addr_hash AS hash;
CREATE DOMAIN staking_addr_hash AS hash;
CREATE DOMAIN script_hash AS hash;


-- NEW for SHELLEY.
-- Base addresses
-- To do: do we also need pointer addresses?  Do we need to distinguish script addresses from other addresses?
CREATE TYPE base_address (
   payment_address payment_addr_hash NOT NULL,
   staking_address staking_addr_hash                -- NULL for bootstrap addresses and enterprise addresses
   );

CREATE TYPE pointer_address (
   payment_address  payment_addr_hash NOT NULL,
   pointer          pointer           NOT NULL
   );

CREATE TYPE address (
   payment_address   payment_addr_hash NOT NULL,
   staking_address   staking_addr_hash,                -- NULL for bootstrap addresses, enterprise addresses and pointer addresses
   pointer_address   pointer                           -- NULL for bootstrap addresses, enterprise addresses and non-pointer addresses
   );

-- To Do: Choose better names for p1, p2, p3
CREATE TYPE pointer (
   p1               uinteger          NOT NULL,
   p2               uinteger          NOT NULL,
   p3               uinteger          NOT NULL
   );

----------------------------------------
--
-- Certificates and Signatures
--
----------------------------------------

-- NEW for SHELLEY: operational certificates.
CREATE TYPE operational_certificate (
    hot_vkey        hash PRIMARY KEY,               -- KES key hash
    sequence_number uinteger NOT NULL,
    kes_period      uinteger NOT NULL,
    sigma           signature
  );


-- NEW for SHELLEY: VRF certificates. To do: verify the size of this.

CREATE DOMAIN vrf_certificate AS bytea;


-- NEW for SHELLEY: signatures. -- To do: verify the size of this.

CREATE DOMAIN signature AS bytea;



----------------------------------------
--
-- Other Types used with Transactions
--
----------------------------------------

-- Transactions contain lists of input and lists of outputs.
-- So we can identify these elements by the 0-based index within the list.
-- To do: confirm limit
CREATE DOMAIN txindex AS smallint
  CHECK (VALUE >= 0 AND VALUE < 1024);

-- NEW for SHELLEY: protocol version. To do: confirm that this is needed in the database
CREATE TYPE protocol_version (
  major     uinteger,
  minor     uinteger
  );


----------------------------------------
--
-- Schema Version
--
----------------------------------------

-- Schema version number. There should only ever be a single row in this table.
-- UNCHANGED FOR SHELLEY
CREATE TABLE version (
  number      uinteger      -- should this be NOT NULL?
);



----------------------------------------
--
-- Blocks
--
----------------------------------------


-- SHELLEY only - no NULL blocks
CREATE TABLE blocks (
  blockid          hash                 PRIMARY KEY,    -- PRIMARY KEY implies UNIQUE and NOT NULL.
  header           block_header         NOT NULL,
  );


-- Could be rolled into block
CREATE TYPE block_header (
   -- header body
  slot_no          uinteger          NOT NULL,
  epoch_no         uinteger         NOT NULL,

  block_no         uinteger          NOT NULL,

  previous         hash
      REFERENCES blocks (blockid),                    -- Needs to be NULL-able because the first block has a previous
                                                      -- hash which does not exist in this table.

  block_issuer     hash              NOT NULL,				-- SHELLEY: Which pool produced the block.
  vrfKey           hash              NOT NULL,        -- SHELLEY: VRF verification key

  nonce_vrf        vrf_certificate   NOT NULL,        -- SHELLEY: nonce proof
  leader_vrf       vef_certificate   NOT NULL,        -- SHELLEY: leader election value

  op_cert          operational_certificate NOT NULL,  -- SHELLEY: operational certificate
  proto_version    protocol_version  NOT NULL,        -- SHELLEY: protocol version

  merkle_root      hash,                              -- Could be NULL-able in Byron era blocks for EBBs

  size             uinteger          NOT NULL,        -- Block size in bytes.

   -- signature
   body_signature signature          NOT NULL         -- KES signature
);


----------------------------------------
--
-- Transactions
--
----------------------------------------


-- Transaction Body
-- ADDED metadata/hash and certificates for SHELLEY.
-- TTL (time to live) is not included since the transaction will only be on the chain if it has succeeded
-- May need to add payment verification key map, and script map.
-- Note that if one of tx_md_hash and tx_metadata is NULL, then so will the other.
-- Optional protocol parameter updates are not included - they are recorded separately
-- Do we need withdrawals in the database?

CREATE TABLE txs (
  txid            hash                   PRIMARY KEY,
  in_blockid      hash                   NOT NULL REFERENCES blocks (blockid) ON DELETE CASCADE,

  fee             lovelace               NOT NULL,
  withdrawals     withdrawal ARRAY,
  metadata        tx_metadata
);



-- NEW for SHELLEY: transaction metadata.
-- For simplicity, we store the metadata as JSON.  This will need to be converted when stored

CREATE TYPE tx_metadata (
    label         uinteger         NOT NULL,
    metadata      jsonb            NOT NULL
    );


-- Transaction output
-- output address changed to (base) address type FOR SHELLEY

CREATE TABLE txouts (
  in_txid      hash                REFERENCES txs (txid) ON DELETE CASCADE,
  index        txindex,
  address      address             NOT NULL,
  value        lovelace            NOT NULL,

  PRIMARY KEY (in_txid, index)
);


-- Transaction input
-- UNCHANGED FOR SHELLEY

CREATE TABLE txins (
  in_txid      hash               REFERENCES txs (txid) ON DELETE CASCADE,
  index        txindex,
  txout_txid   hash               NOT NULL,
  txout_index  txindex            NOT NULL,

  PRIMARY KEY (in_txid, index),
  FOREIGN KEY (txout_txid, txout_index)
    REFERENCES txouts (in_txid, index)
);


-- UTxO
-- UNCHANGED FOR SHELLEY

CREATE TABLE utxo (
  txid         hash ,
  index        txindex,

  PRIMARY KEY (txid, index),
  FOREIGN KEY (txid, index)
    REFERENCES txouts (in_txid, index)
);


-- Resolved transaction input - which transaction inputs appear in the transaction output
-- UNCHANGED FOR SHELLEY

CREATE VIEW txins_resolved AS
  SELECT txins.*,
         address,
         value
  FROM txins INNER JOIN txouts ON
       (txins.txout_txid  = txouts.in_txid AND
        txins.txout_index = txouts.index);


-- Resolved UTXO - which UTXOs appear in the transaction output
-- UNCHANGED FOR SHELLEY

CREATE VIEW utxo_resolved AS
  SELECT utxo.*,
         address,
         value
  FROM utxo INNER JOIN txouts ON
       (utxo.txid  = txouts.in_txid AND
        utxo.index = txouts.index);


----------------------------------------
--
-- Withdrawals - used to record deductions from rewards for accounting purposes
--
----------------------------------------


-- NEW for Shelley
-- Merges key and script hashs

CREATE TYPE withdrawal (
       from            hash           NOT NULL         -- may be either script or key
       amount          lovelace       NOT NULL         -- amount withdrawn
   );


----------------------------------------
--
-- Treasury and Reserves
--
----------------------------------------


-- NEW for SHELLEY
-- This records the total value of the treasury and reserves at the beginning of the specified epoch.

CREATE TABLE central_funds (
   epoch_no           uinteger        PRIMARY KEY,
   treasury           lovelace        NOT NULL,
   reserves           lovelace        NOT NULL
   );


----------------------------------------
--
-- Rewards and Delegation
--
----------------------------------------


-- NEW for SHELLEY
-- Basic rewards data.
-- What reward has been earned in the epoch by delegating to the specified pool id.
-- This design allows rewards to be discriminated based on how they are earned.

CREATE TABLE rewards (
   rewards_address      staking_addr_hash NOT NULL,
   instantaneous        boolean           NOT NULL,
   pool_id              hash              NOT NULL,                         -- Arbitrary if an instantaneous reward - needed for primary key
   epoch_no             uinteger          NOT NULL,
   reward               lovelace          NOT NULL,

   PRIMARY KEY (rewards_address,pool_id,epoch_no,instantaneous)
   );


-- NEW for SHELLEY
-- Basic delegation data.
-- What has been delegated to a pool and when.  Where will the rewards be recorded.
-- The epoch can be deduced from the unique slot number.
-- Note the overlap with the rewards table in the first two fields.

CREATE TABLE delegation (
   rewards_address      staking_addr_hash  NOT NULL,
   pool_id              hash               NOT NULL,
   slot_no              uinteger           NOT NULL,      -- technically redundant, but useful
   amount               lovelace           NOT NULL,
   in_tx                hash               NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (rewards_address,pool_id,in_tx)
   );


-- NEW for SHELLEY
-- When was a staking key registered

CREATE TABLE stake_key_registration (
   stakekey_address     staking_addr_hash NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (stake_key_hash,in_tx)
   );


-- NEW for SHELLEY
-- When was a staking key unregistered

CREATE TABLE stake_key_deregistration (
   stakekey_address     staking_addr_hash PRIMARY KEY,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (stake_key_hash,in_tx)
   );


-- NEW for SHELLEY
-- When was a script registered

CREATE TABLE script_registration (
   script_address       script_hash       NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (script_address,in_tx)
   );


-- NEW for SHELLEY
-- When was a script unregistered

CREATE TABLE script_deregistration (
   script_address       script_hash       NOT NULL,
   slot_no              uinteger          NOT NULL,
   in_tx                hash              NOT NULL REFERENCES txs (txid) ON DELETE CASCADE,

   PRIMARY KEY (script_address,in_tx)
   );


-- NEW for SHELLEY
-- Parameter update - there can only be one successful update per epoch.

CREATE TABLE parameter_update (
   epoch_no             uinteger          PRIMARY KEY,
   min_fee              uinteger          NOT NULL,
   max_fee              uinteger          NOT NULL,
   max_block_size       uinteger          NOT NULL,
   max_tx_size          uinteger          NOT NULL,
   max_bh_size          uinteger          NOT NULL,
   key_deposit          lovelace          NOT NULL,
   key_deposit_refund   interval          NOT NULL,
   key_deposit_decay    rational          NOT NULL,
   pool_deposit         lovelace          NOT NULL,
   pool_deposit_refund  interval          NOT NULL,
   pool_deposit_decay   rational          NOT NULL,
   max_epoch            uinteger          NOT NULL,
   n_optimal            uinteger          NOT NULL,
   influence            rational          NOT NULL,
   monetary_expansion_rate interval       NOT NULL,
   treasury_growth_rate interval          NOT NULL,
   active_slot_coeff    interval          NOT NULL,
   decentralisation     interval          NOT NULL,
   entropy              nonce,                              -- NULL if the nonce is not present
   protocol_version     protocol_version  NOT NULL
   );




----------------------------------------
--
-- Pool Specific Information
--
----------------------------------------


-- Tickers may be 1-5 bytes
CREATE DOMAIN ticker AS bytea
  CHECK (octet_length (VALUE) = 5);

-- URL for pool description.
CREATE DOMAIN url AS bytea
  CHECK (octet_length (VALUE) = 64);

-- Percentages in the range 0%-100% -- recorded to two decimal places
CREATE DOMAIN percentage AS decimal (5,2)
  CHECK (VALUE >= 0 AND VALUE <= 100);


-- NEW for SHELLEY
-- Pool Parameters.
-- Records important pool-specific data.

CREATE TABLE pool_params (
   pool_id              hash              NOT NULL,          -- hash of the pool VRF key
   owners               hash ARRAY        NOT NULL,          -- key hashes for the owners - at least one owner is needed
   ticker_id            ticker            NOT NULL,
   pledge               lovelace          NOT NULL,
x   reward_address       staking_addr_hash NOT NULL,          -- overall pool rewards
   pool_url             url,
   metadata_hash        hash,
   margin               percentage        NOT NULL,
   fixed_cost           uinteger          NOT NULL,
   registered           uinteger          NOT NULL,          -- in which slot was this metadata registered/re-registered

   PRIMARY KEY (pool_id,registered)
   );


-- NEW for SHELLEY
-- Pool Retirement

CREATE TABLE pool_retired (
   pool_id              hash NOT NULL,                        -- hash of the pool VRF key
   retired              uinteger NOT NULL,                    -- when retirement occurred (epoch no)

   PRIMARY KEY (pool_id,retired)
   );


-- NEW for SHELLEY
-- Announcement of Pool Retirement

CREATE TABLE pool_retiring (
   pool_id              hash NOT NULL,                        -- hash of the pool VRF key
   announced            uinteger NOT NULL,                    -- which slot was retirement announced
   retiring             uinteger NOT NULL,                    -- in which epoch is retirement planned

   PRIMARY KEY (pool_id,announced)
  );
