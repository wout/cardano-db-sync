
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.SMASH.DBSync.Db.Insert
  ( insertPool
  , insertPoolMetadata
  , insertReservedTicker
  , insertDelistedPool
  , insertRetiredPool

  -- Export mainly for testing.
  , insertByReturnKey
  ) where

import           Cardano.Prelude hiding (Meta, replace)

import           Control.Monad.Trans.Reader (mapReaderT)

import           Database.Persist.Class (AtLeastOneUniqueKey, PersistEntityBackend, getByValue,
                   insert)
import           Database.Persist.Sql (SqlBackend)
import           Database.Persist.Types (entityKey)
import           Database.PostgreSQL.Simple (SqlError)

import           Cardano.Db
import           Cardano.SMASH.Db.Error

insertPool :: pool -> ReaderT SqlBackend m (Either DBFail poolId)
insertPool = panic "Cardano.SMASH.DBSync.Db.Insert.insertPool"

insertPoolMetadata :: poolMetadata -> ReaderT SqlBackend m (Either DBFail poolMetadataId)
insertPoolMetadata = panic "Cardano.SMASH.DBSync.Db.Insert.insertPoolMetadata"

insertReservedTicker :: reservedTicker -> ReaderT SqlBackend m (Either DBFail reservedTickerId)
insertReservedTicker _ = panic "Cardano.SMASH.DBSync.Db.Insert.insertReservedTicker"
{-
insertReservedTicker :: (MonadIO m) => reservedTicker -> ReaderT SqlBackend m (Either DBFail reservedTickerId)
insertReservedTicker reservedTicker = do
    isUnique <- checkUnique reservedTicker
    -- If there is no unique constraint violated, insert, otherwise return error.
    case isUnique of
        Nothing -> insertByReturnKey reservedTicker
        Just _key -> return . Left . ReservedTickerAlreadyInserted $ show reservedTicker
-}

insertDelistedPool :: delistedPool -> ReaderT SqlBackend m (Either DBFail delistedPoolId)
insertDelistedPool _ = panic "Cardano.SMASH.DBSync.Db.Insert.insertDelistedPool"
{-
insertDelistedPool :: (MonadIO m) => delistedPool -> ReaderT SqlBackend m (Either DBFail delistedPoolId)
insertDelistedPool delistedPool = do
    isUnique <- checkUnique delistedPool
    -- If there is no unique constraint violated, insert, otherwise return error.
    case isUnique of
        Nothing -> insertByReturnKey delistedPool
        Just _key -> return . Left . DbInsertError $ "Delisted pool already exists!"
-}

insertRetiredPool :: retiredPool -> ReaderT SqlBackend m (Either DBFail retiredPoolId)
insertRetiredPool = panic "Cardano.SMASH.DBSync.Db.Insert.insertRetiredPool"

-------------------------------------------------------------------------------

-- | Insert a record (with a Unique constraint), and return 'Right key' if the
-- record is inserted and 'Left key' if the record already exists in the DB.
-- TODO(KS): This needs to be tested, not sure if it's actually working.
insertByReturnKey
    :: ( AtLeastOneUniqueKey record
       , MonadIO m
       , PersistEntityBackend record ~ SqlBackend
       )
    => record -> ReaderT SqlBackend m (Either DBFail (Key record))
insertByReturnKey value = do
    res <- getByValue value
    case res of
        -- handle :: Exception e => (e -> IO a) -> IO a -> IO a
        Nothing -> mapReaderT (liftIO . handle exceptionHandler) (Right <$> insert value)
        Just r  -> pure . Right $ entityKey r
  where
    exceptionHandler :: MonadIO m => SqlError -> m (Either DBFail a)
    exceptionHandler e =
        liftIO . pure . Left . DbInsertError . show $ e
