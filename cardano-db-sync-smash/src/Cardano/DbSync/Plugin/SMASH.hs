{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.DbSync.Plugin.SMASH
  ( runSmashWebapp
  ) where

import           Cardano.Prelude

import           Cardano.BM.Trace (Trace, logInfo)


import           Cardano.SMASH.DB (DataLayer (..))
import           Cardano.SMASH.Lib

import           Database.Persist.Sql (SqlBackend)


runSmashWebapp :: DataLayer -> SqlBackend -> Trace IO Text -> IO (Either a ())
runSmashWebapp dataLayer _backend tracer = do
  liftIO . logInfo tracer $ "runSmashWebapp: Starting the web API"
  Right <$> runApp tracer dataLayer defaultConfiguration

