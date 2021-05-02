{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.DB
    ( module X
    , DataLayer (..)
    , PoolMetadata (..)
    , postgresqlDataLayer
    ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace)

-- import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
-- import qualified Data.Map as Map
import           Data.Time.Clock (UTCTime)
-- import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import           Database.Persist.Sql (SqlBackend)

import           Cardano.SMASH.DBSync.Db.Delete (deleteAdminUser, deleteDelistedPool,
                   deleteRetiredPool)
import           Cardano.SMASH.DBSync.Db.Insert (insertPoolMetadata)
-- import           Cardano.SMASH.DBSync.Db.Insert (insertDelistedPool, insertPool, insertPoolMetadata,
--                   insertReservedTicker, insertRetiredPool)
import           Cardano.SMASH.DBSync.Db.Query
import           Cardano.SMASH.Types

import           Cardano.Db as X
import           Cardano.SMASH.Db.Error as X

data PoolMetadata = PoolMetadata
  { poolId :: !PoolIdent
  , tickerName :: !TickerName
  , hash :: !PoolMetaHash
  , metadata :: !PoolMetadataRaw
  , pmrId :: !(Maybe PoolMetadataRefId)
  }

-- | This is the data layer for the DB.
-- The resulting operation has to be @IO@, it can be made more granular,
-- but currently there is no complexity involved for that to be a sane choice.
data DataLayer = DataLayer
    { dlGetPoolMetadata         :: PoolIdent -> PoolMetaHash -> IO (Either DBFail (TickerName, PoolMetadataRaw))
    , dlGetAllPoolMetadata      :: IO [PoolMetadata]
    , dlAddPoolMetadata         :: Maybe PoolMetadataRefId -> PoolIdent -> PoolMetaHash -> PoolMetadataRaw -> PoolTicker -> IO (Either DBFail PoolMetadataRaw)

    , dlAddMetaDataReference    :: PoolIdent -> PoolUrl -> PoolMetaHash -> IO (Either DBFail PoolMetadataRefId)

    , dlGetReservedTickers      :: IO [(TickerName, PoolMetaHash)]
    , dlAddReservedTicker       :: TickerName -> PoolMetaHash -> IO (Either DBFail TickerName)
    , dlCheckReservedTicker     :: TickerName -> PoolMetaHash -> IO (Maybe TickerName)

    , dlGetDelistedPools        :: IO [PoolIdent]
    , dlCheckDelistedPool       :: PoolIdent -> IO Bool
    , dlAddDelistedPool         :: PoolIdent -> IO (Either DBFail PoolIdent)
    , dlRemoveDelistedPool      :: PoolIdent -> IO (Either DBFail PoolIdent)

    , dlAddRetiredPool          :: PoolIdent -> Word64 -> IO (Either DBFail PoolIdent)
    , dlCheckRetiredPool        :: PoolIdent -> IO (Either DBFail (PoolIdent, Word64))
    , dlGetRetiredPools         :: IO (Either DBFail [PoolIdent])
    , dlRemoveRetiredPool       :: PoolIdent -> IO (Either DBFail PoolIdent)

    , dlGetAdminUsers           :: IO (Either DBFail [AdminUser])
    , dlAddAdminUser            :: ApplicationUser -> IO (Either DBFail AdminUser)
    , dlRemoveAdminUser         :: ApplicationUser -> IO (Either DBFail AdminUser)

    -- TODO(KS): Switch to PoolFetchError!
    , dlAddFetchError           :: PoolOfflineFetchError -> IO (Either DBFail PoolOfflineFetchErrorId)
    , dlGetFetchErrors          :: PoolIdent -> Maybe UTCTime -> IO (Either DBFail [PoolFetchError])

    , dlGetPool                 :: PoolIdent -> IO (Either DBFail PoolIdent)
    , dlAddPool                 :: PoolIdent -> IO (Either DBFail PoolIdent)

    } deriving (Generic)


-- TODO(KS): Passing the optional tracer.
postgresqlDataLayer :: SqlBackend -> Trace IO Text -> DataLayer
postgresqlDataLayer sqlBackend tracer = DataLayer
    { dlGetPoolMetadata = \pId poolMetadataHash' -> do
        poolMetadata <- runDbIohkLogging sqlBackend tracer $ queryPoolMetadata pId poolMetadataHash'
        let poolTickerName = tickerName <$> poolMetadata
        let poolMetadata' = metadata <$> poolMetadata
        pure $ (,) <$> poolTickerName <*> poolMetadata'
    , dlGetAllPoolMetadata = runDbIohkLogging sqlBackend tracer queryAllPoolMetadata
    , dlAddPoolMetadata     = \mRefId pId poolHash poolMetadata poolTicker -> do
        let poolTickerName = TickerName $ unPoolTicker poolTicker
        poolMetadataId <- runDbIohkLogging sqlBackend tracer $ insertPoolMetadata $ PoolMetadata pId poolTickerName poolHash poolMetadata mRefId

        case poolMetadataId of
            Left err  -> pure $ Left err
            Right _id -> pure $ Right poolMetadata

    , dlAddMetaDataReference = panic "Cardano.SMASH.DB.dlAddMetaDataReference"
    {-
    , dlAddMetaDataReference = \poolId poolUrl poolMetadataHash' ->
        runDbIohkLogging sqlBackend tracer $ insertPoolMetadataRef $
            PoolMetadataRef
                { poolMetadataRefUrl = poolUrl
                , poolMetadataRefHash = panic "Cardano.SMASH.DB.dlAddMetaDataReference"
                , poolMetadataRefPoolId = panic "Cardano.SMASH.DB.dlAddMetaDataReference"
                , poolMetadataRefRegisteredTxId = panic "Cardano.SMASH.DB.dlAddMetaDataReference"
                }
    -}
    , dlGetReservedTickers = panic "Cardano.SMASH.DB.dlGetReservedTickers"
    {-
    , dlGetReservedTickers = do
        reservedTickers <- runDbIohkLogging sqlBackend tracer queryAllReservedTickers
        pure $ map (\reservedTicker -> (reservedTickerName reservedTicker, reservedTickerPoolHash reservedTicker)) reservedTickers
    -}
    , dlAddReservedTicker = panic "Cardano.SMASH.DB.dlAddReservedTicker"
    {-
    , dlAddReservedTicker = \tickerName poolMetadataHash' -> do
        reservedTickerId <- runDbIohkLogging sqlBackend tracer $ insertReservedTicker $ ReservedTicker tickerName poolMetadataHash'

        case reservedTickerId of
            Left err  -> pure $ Left err
            Right _id -> pure $ Right tickerName
    -}
    , dlCheckReservedTicker = \tickName poolMetadataHash' -> do
        mReservedTicker <- runDbIohkLogging sqlBackend tracer $ queryReservedTicker tickName poolMetadataHash'

        case mReservedTicker of
            Nothing              -> pure Nothing
            Just _reservedTicker -> pure $ Just tickName

    , dlGetDelistedPools = panic "Cardano.SMASH.DB.dlGetDelistedPools"
    {-
    , dlGetDelistedPools = do
        delistedPoolsDB <- runDbIohkLogging sqlBackend tracer queryAllDelistedPools
        -- Convert from DB-specific type to the "general" type
        pure $ map (PoolIdent . getPoolIdent . delistedPoolPoolId) delistedPoolsDB
    -}

    , dlCheckDelistedPool = \pId -> do
        runDbIohkLogging sqlBackend tracer $ queryDelistedPool pId

    , dlAddDelistedPool  = panic "Cardano.SMASH.DB.dlAddDelistedPool"
    {-
    , dlAddDelistedPool  = \poolId -> do
        delistedPoolIdent <- runDbIohkLogging sqlBackend tracer $ insertDelistedPool $ DelistedPool poolId

        case delistedPoolIdent of
            Left err  -> pure $ Left err
            Right _id -> pure $ Right poolId
    -}

    , dlRemoveDelistedPool = \pId -> do
        isDeleted <- runDbIohkLogging sqlBackend tracer $ deleteDelistedPool pId
        -- Up for a discussion, but this might be more sensible in the lower DB layer.
        if isDeleted
            then pure $ Right pId
            else pure $ Left RecordDoesNotExist

    , dlAddRetiredPool  = panic "Cardano.SMASH.DB.dlAddRetiredPool"

    {-
    , dlAddRetiredPool  = \poolId blockNo -> do
        retiredPoolId <- runDbIohkLogging sqlBackend tracer $ insertRetiredPool $ RetiredPool poolId blockNo

        case retiredPoolId of
            Left err  -> pure $ Left err
            Right _id -> pure $ Right poolId
    -}

    , dlCheckRetiredPool = panic "Cardano.SMASH.DB.dlCheckRetiredPool"

    {-
    , dlCheckRetiredPool = \poolId -> do
        retiredPool <- runDbIohkLogging sqlBackend tracer $ queryRetiredPool poolId
        case retiredPool of
            Left err -> pure $ Left err
            Right retiredPool' -> pure $ Right (retiredPoolPoolId retiredPool', retiredPoolBlockNo retiredPool')
    -}

    , dlGetRetiredPools = do
        retiredPools <- runDbIohkLogging sqlBackend tracer queryAllRetiredPools
        pure $ Right retiredPools

    , dlRemoveRetiredPool = \pId -> do
        isDeleted <- runDbIohkLogging sqlBackend tracer $ deleteRetiredPool pId
        if isDeleted
            then pure $ Right pId
            else pure $ Left $ UnknownError "Retired pool not deleted!"

    , dlGetAdminUsers       = do
        adminUsers <- runDbIohkLogging sqlBackend tracer queryAdminUsers
        pure $ Right adminUsers
    , dlAddAdminUser        = \(ApplicationUser user pass') -> do
        let adminUser = AdminUser user pass'
        _adminUserId <- runDbIohkLogging sqlBackend tracer $ insertAdminUser adminUser
        pure $ Right adminUser
    , dlRemoveAdminUser     = \(ApplicationUser user pass') -> do
        let adminUser = AdminUser user pass'
        isDeleted <- runDbIohkLogging sqlBackend tracer $ deleteAdminUser adminUser
        if isDeleted
            then pure $ Right adminUser
            else pure $ Left $ UnknownError "Admin user not deleted. Both username and password must match."


    , dlAddFetchError       = panic "Cardano.SMASH.DB.dlAddFetchError"
    {-
    , dlAddFetchError       = runDbIohkLogging sqlBackend tracer . insertPoolOfflineFetchError
    -}

    , dlGetFetchErrors      = \pId mTimeFrom -> do
        poolMetadataFetchErrors <- runDbIohkLogging sqlBackend tracer (queryPoolOfflineFetchErrorByTime pId mTimeFrom)
        pure . sequence $ Right <$> map convertPoolOfflineFetchError poolMetadataFetchErrors

    , dlGetPool             = \pId -> do
        pool <- runDbIohkLogging sqlBackend tracer $ queryPoolByPoolId pId
        case pool of
            Left err   -> pure $ Left err
            Right _val -> pure $ Right pId

    , dlAddPool = panic "Cardano.SMASH.DB.dlAddPool"
    {-
    , dlAddPool             = \poolId -> do
        poolId' <- runDbIohkLogging sqlBackend tracer $ insertPool (Pool poolId)
        case poolId' of
            Left err   -> pure $ Left err
            Right _val -> pure $ Right poolId
    -}
    }

convertPoolOfflineFetchError :: PoolOfflineFetchError -> PoolFetchError
convertPoolOfflineFetchError = panic "Cardano.SMASH.DB.convertPoolOfflineFetchError"
{-
convertPoolOfflineFetchError (PoolOfflineFetchError poolId timeUTC poolHash _pMRId fetchError retryCount) =
    PoolFetchError (utcTimeToPOSIXSeconds timeUTC) poolId poolHash fetchError retryCount
-}


{-
    poolId              PoolHashId          OnDeleteCascade
    fetchTime           UTCTime             sqltype=timestamp
    pmrId               PoolMetadataRefId   OnDeleteCascade
    fetchError          Text
    retryCount          Word                sqltype=uinteger
-}
