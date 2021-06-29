{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.Offline
  ( fetchInsertNewPoolMetadata
  , fetchInsertNewPoolMetadataOld
  , runOfflineFetchThread
  ) where

import           Cardano.Prelude hiding (from, groupBy, on, retry)

import           Cardano.BM.Trace (Trace)

import           Cardano.SMASH.DB (DataLayer (..))
import           Cardano.SMASH.FetchQueue

import           Cardano.Db
import qualified Cardano.Db as DB
import           Cardano.DbSync.Era.Shelley.Offline.Http (FetchError (..))

import           Database.Persist.Sql (SqlBackend)

import qualified Shelley.Spec.Ledger.TxBody as Shelley

fetchInsertNewPoolMetadata
    :: DataLayer
    -> Trace IO Text
    -> DB.PoolMetadataRefId
    -> PoolIdent
    -> Shelley.PoolMetadata
    -> IO ()
fetchInsertNewPoolMetadata =
  panic "Cardano.SMASH.Offline.fetchInsertNewPoolMetadata"

fetchInsertNewPoolMetadataOld
    :: DataLayer
    -> Trace IO Text
    -> (DataLayer -> PoolIdent -> Trace IO Text -> PoolFetchRetry -> ExceptT FetchError IO ())
    -> PoolFetchRetry
    -> IO PoolFetchRetry
fetchInsertNewPoolMetadataOld =
  panic "Cardano.SMASH.Offline.fetchInsertNewPoolMetadataOld"

runOfflineFetchThread :: SqlBackend -> Trace IO Text -> IO ()
runOfflineFetchThread =
  panic "Cardano.SMASH.Offline.runOfflineFetchThread"


