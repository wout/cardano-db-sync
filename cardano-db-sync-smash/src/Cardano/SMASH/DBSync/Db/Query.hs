{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.SMASH.DBSync.Db.Query
  ( DBFail (..)
  , queryAllPools
  , queryPoolByPoolId
  , queryAllPoolMetadata
  , queryPoolMetadata
  , queryDelistedPool
  , queryAllDelistedPools
  , queryAllReservedTickers
  , queryReservedTicker
  , queryAdminUsers
  , queryPoolOfflineFetchError
  , queryPoolOfflineFetchErrorByTime
  , queryAllRetiredPools
  , queryRetiredPool
  ) where

import           Cardano.Prelude hiding (Meta, from, maybeToEither, on)

import           Data.Time.Clock (UTCTime)

import           Database.Esqueleto (InnerJoin (..), desc, entityVal, from, limit, on, orderBy,
                   select, val, where_, (==.), (>=.), (^.))
import           Database.Persist.Sql (SqlBackend, selectList)

import           Cardano.Db
import           Cardano.SMASH.Db.Error
import           Cardano.Sync.Types (PoolMetaHash (..))

-- |Return all pools.
queryAllPools :: ReaderT SqlBackend m [pool]
queryAllPools = panic "Cardano.SMASH.DBSync.Db.Query.queryAllPools"
{-
queryAllPools = do
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- |Return pool, that is not RETIRED!
queryPoolByPoolId :: PoolIdent -> ReaderT SqlBackend m (Either DBFail pool)
queryPoolByPoolId _ = panic "Cardano.SMASH.DBSync.Db.Query.queryPoolByPoolId"

{-
queryPoolByPoolId poolId = do
  res <- select . from $ \(pool :: SqlExpr (Entity Pool)) -> do
            where_ (pool ^. PoolPoolId ==. val poolId
                &&. pool ^. PoolPoolId `notIn` retiredPoolsPoolId)
            pure pool
  pure $ maybeToEither RecordDoesNotExist entityVal (listToMaybe res)
  where
    -- |Subselect that selects all the retired pool ids.
    retiredPoolsPoolId :: SqlExpr (ValueList PoolIdent)
    retiredPoolsPoolId =
        subList_select . from $ \(retiredPool :: SqlExpr (Entity RetiredPool)) ->
        return $ retiredPool ^. RetiredPoolPoolId
-}

-- |Return all retired pools.
queryAllPoolMetadata :: ReaderT SqlBackend m [poolMetadata]
queryAllPoolMetadata = panic "Cardano.SMASH.DBSync.Db.Query.queryAllPoolMetadata"
{-
queryAllPoolMetadata :: MonadIO m => ReaderT SqlBackend m [PoolMetadata]
queryAllPoolMetadata = do
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- | Get the 'Block' associated with the given hash.
-- We use the @PoolIdent@ to get the nice error message out.
queryPoolMetadata :: PoolIdent -> PoolMetaHash -> ReaderT SqlBackend m (Either DBFail poolMetadata)
queryPoolMetadata _ _ = panic "Cardano.SMASH.DBSync.Db.Query.queryPoolMetadata"
{-
queryPoolMetadata :: MonadIO m => PoolIdent -> PoolMetaHash -> ReaderT SqlBackend m (Either DBFail PoolMetadata)
queryPoolMetadata poolId poolMetadataHash' = do
  res <- select . from $ \ poolMetadata -> do
            where_ (poolMetadata ^. PoolMetadataPoolId ==. val poolId
                &&. poolMetadata ^. PoolMetadataHash ==. val poolMetadataHash')
            pure poolMetadata
  pure $ maybeToEither (DbLookupPoolMetadataHash poolId poolMetadataHash') entityVal (listToMaybe res)
-}

-- |Return all retired pools.
queryAllRetiredPools :: ReaderT SqlBackend m [a]
queryAllRetiredPools = panic "Cardano.SMASH.DBSync.Db.Query.queryAllRetiredPool"
{-
queryAllRetiredPools :: MonadIO m => ReaderT SqlBackend m [RetiredPool]
queryAllRetiredPools = do
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- |Query retired pools.
queryRetiredPool :: PoolIdent -> ReaderT SqlBackend m (Either DBFail retiredPool)
queryRetiredPool _ = panic "Cardano.SMASH.DBSync.Db.Query.queryRetiredPool"
{-
queryRetiredPool :: MonadIO m => PoolIdent -> ReaderT SqlBackend m (Either DBFail RetiredPool)
queryRetiredPool poolId = do
  res <- select . from $ \retiredPools -> do
            where_ (retiredPools ^. RetiredPoolPoolId ==. val poolId)
            pure retiredPools
  pure $ maybeToEither RecordDoesNotExist entityVal (listToMaybe res)
-}

-- | Check if the hash is in the table.
queryDelistedPool :: PoolIdent -> ReaderT SqlBackend m Bool
queryDelistedPool _ = panic "Cardano.SMASH.DBSync.Db.Query.queryDelistedPool"
{-
queryDelistedPool poolId = do
  res <- select . from $ \(pool :: SqlExpr (Entity DelistedPool)) -> do
            where_ (pool ^. DelistedPoolPoolId ==. val poolId)
            pure pool
  pure $ Data.Maybe.isJust (listToMaybe res)
-}

-- |Return all delisted pools.
queryAllDelistedPools :: ReaderT SqlBackend m [delistedPool]
queryAllDelistedPools = panic "Cardano.SMASH.DBSync.Db.Query.queryAllDelistedPools"
{-
queryAllDelistedPools :: MonadIO m => ReaderT SqlBackend m [DelistedPool]
queryAllDelistedPools = do
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- |Return all reserved tickers.
queryAllReservedTickers :: ReaderT SqlBackend m [reservedTicker]
queryAllReservedTickers = panic "Cardano.SMASH.DBSync.Db.Query.queryAllReservedTickers"
{-
queryAllReservedTickers :: MonadIO m => ReaderT SqlBackend m [ReservedTicker]
queryAllReservedTickers = do
  res <- selectList [] []
  pure $ entityVal <$> res
-}

-- | Check if the ticker is in the table.
queryReservedTicker :: TickerName -> PoolMetaHash -> ReaderT SqlBackend m (Maybe reservedTicker)
queryReservedTicker _ _ = panic "Cardano.SMASH.DBSync.Db.Query.queryReservedTicker"
{-
queryReservedTicker :: MonadIO m => TickerName -> PoolMetaHash -> ReaderT SqlBackend m (Maybe ReservedTicker)
queryReservedTicker reservedTickerName' poolMetadataHash' = do
  res <- select . from $ \(reservedTicker :: SqlExpr (Entity ReservedTicker)) -> do
            where_ (reservedTicker ^. ReservedTickerName ==. val reservedTickerName'
                &&. reservedTicker ^. ReservedTickerPoolHash ==. val poolMetadataHash')

            limit 1
            pure reservedTicker
  pure $ fmap entityVal (listToMaybe res)
-}

-- | Query all admin users for authentication.
queryAdminUsers :: MonadIO m => ReaderT SqlBackend m [AdminUser]
queryAdminUsers = do
  res <- selectList [] []
  pure $ entityVal <$> res

-- | Query all the errors we have.
queryPoolOfflineFetchError :: MonadIO m => Maybe PoolIdent -> ReaderT SqlBackend m [PoolOfflineFetchError]
queryPoolOfflineFetchError mPoolIdent = do
  res <- select . from $ \(pmfe `InnerJoin` ph) -> do
            on (pmfe ^. PoolOfflineFetchErrorPoolId ==. ph ^. PoolHashId)
            case mPoolIdent of
              Nothing -> pure ()
              Just (PoolIdent poolIdent) -> where_ (ph ^. PoolHashView ==. val poolIdent)
            pure pmfe
  pure $ fmap entityVal res

-- We currently query the top 10 errors (chronologically) when we don't have the time parameter, but we would ideally
-- want to see the top 10 errors from _different_ pools (group by), using something like:
-- select pool_id, pool_hash, max(retry_count) from pool_metadata_fetch_error group by pool_id, pool_hash;
queryPoolOfflineFetchErrorByTime
    :: MonadIO m
    => PoolIdent -> Maybe UTCTime
    -> ReaderT SqlBackend m [PoolOfflineFetchError]
queryPoolOfflineFetchErrorByTime (PoolIdent poolIdent) mFromTime = do
  res <- select . from $ \(pmfe `InnerJoin` ph) -> do
            on (pmfe ^. PoolOfflineFetchErrorPoolId ==. ph ^. PoolHashId)
            where_ (ph ^. PoolHashView ==. val poolIdent)
            case mFromTime of
              Nothing -> pure ()
              Just fromTime -> where_ (pmfe ^. PoolOfflineFetchErrorFetchTime >=. val fromTime)
            orderBy [desc (pmfe ^. PoolOfflineFetchErrorFetchTime)]
            limit 10
            pure pmfe
  pure $ fmap entityVal res
