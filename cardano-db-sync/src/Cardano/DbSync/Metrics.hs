{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.DbSync.Metrics
  ( Metrics (..)
  , makeMetrics
  , withMetricsLayer
  , withMetricsServer
  ) where

import           Cardano.Prelude

import           Cardano.Sync.Types (MetricsLayer (..))

import           System.Metrics.Prometheus.Concurrent.RegistryT (RegistryT (..), registerGauge,
                   runRegistryT, unRegistryT)
import           System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import           System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge

-- |The metrics we use for Prometheus.
data Metrics = Metrics
  { mNodeBlockHeight :: !Gauge
  -- ^ The block tip number of the remote node.
  , mDbQueueLength :: !Gauge
  -- ^ The number of @DbAction@ remaining for the database.
  , mDbBlockHeight :: !Gauge
  -- ^ The block tip number in the database.
  , mDbSlotHeight :: !Gauge
  -- ^ The slot tip number in the database.
  }

-- This enables us to be much more flexibile with what we actually measure.
withMetricsLayer :: Int -> (MetricsLayer -> IO a) -> IO a
withMetricsLayer prometheusPort action =
    withMetricsServer prometheusPort $ \metrics -> do

        -- Metrics layer.
        let metricsLayer =
                MetricsLayer
                    { mlSetNodeBlockHeight = \nodeHeight ->
                        Gauge.set (fromIntegral nodeHeight) $ mNodeBlockHeight metrics
                    , mlSetDbQueueLength = \queuePostWrite ->
                        Gauge.set (fromIntegral queuePostWrite) $ mDbQueueLength metrics
                    , mlSetDbBlockHeight = \blockNo ->
                        Gauge.set (fromIntegral blockNo) $ mDbBlockHeight metrics
                    , mlSetDbSlotHeight = \slotNo ->
                        Gauge.set (fromIntegral slotNo) $ mDbSlotHeight metrics
                    }

        action metricsLayer

withMetricsServer :: Int -> (Metrics -> IO a) -> IO a
withMetricsServer port action = do
  (metrics, registry) <- runRegistryT $ (,) <$> makeMetrics <*> RegistryT ask
  bracket
     (async $ runReaderT (unRegistryT $ serveMetricsT port []) registry)
     cancel
     (const $ action metrics)

makeMetrics :: RegistryT IO Metrics
makeMetrics = do
    nodeBlockHeight <- registerGauge "cardano_db_sync_node_block_height" mempty
    dbQueueLength <- registerGauge "cardano_db_sync_db_queue_length" mempty
    dbBlockHeight <- registerGauge "cardano_db_sync_db_block_height" mempty
    dbSlotHeight <- registerGauge "cardano_db_sync_db_slot_height" mempty

    return $ Metrics
        { mNodeBlockHeight = nodeBlockHeight
        , mDbQueueLength = dbQueueLength
        , mDbBlockHeight = dbBlockHeight
        , mDbSlotHeight = dbSlotHeight
        }
