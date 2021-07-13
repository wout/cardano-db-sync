{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Sync.Util
  ( cardanoBlockSlotNo
  , fmap3
  , getSyncStatus
  , isSyncedWithinSeconds
  , liftedLogException
  , logException
  , panicAbort
  , renderByteArray
  , renderPoint
  , renderSlotList
  , textPrettyShow
  , textShow
  , tipBlockNo
  , traverseMEither
  , nullMetricSetters
  , maybeToStrict
  , whenJust
  , thrd3
  ) where

import           Cardano.Prelude hiding (catch)

import           Cardano.BM.Trace (Trace, logError)

import           Cardano.Db (SyncState (..))

import           Cardano.Sync.Config.Types ()
import           Cardano.Sync.Types

import           Cardano.Slotting.Slot (SlotNo (..), WithOrigin (..), withOrigin)

import           Control.Exception.Lifted (catch)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.ByteArray (ByteArrayAccess)
import qualified Data.ByteArray
import qualified Data.ByteString.Base16 as Base16
import qualified Data.List as List
import qualified Data.Strict.Maybe as Strict
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import           Data.Time (diffUTCTime)

import           Text.Show.Pretty (ppShow)

import           Ouroboros.Consensus.Block.Abstract (ConvertRawHash (..))
import           Ouroboros.Network.Block (BlockNo (..), Tip, blockSlot, getPoint, getTipBlockNo)
import qualified Ouroboros.Network.Point as Point

import           System.IO.Unsafe (unsafePerformIO)
import           System.Posix.Process (exitImmediately)

cardanoBlockSlotNo :: CardanoBlock -> SlotNo
cardanoBlockSlotNo = blockSlot

fmap3 :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

getSyncStatus :: SlotDetails -> SyncState
getSyncStatus sd = isSyncedWithinSeconds sd 120

isSyncedWithinSeconds :: SlotDetails -> Word -> SyncState
isSyncedWithinSeconds sd target =
  -- diffUTCTime returns seconds.
  let secDiff = ceiling (diffUTCTime (sdCurrentTime sd) (sdSlotTime sd)) :: Int
  in if fromIntegral (abs secDiff) <= target
        then SyncFollowing
        else SyncLagging

textPrettyShow :: Show a => a -> Text
textPrettyShow = Text.pack . ppShow

textShow :: Show a => a -> Text
textShow = Text.pack . show

tipBlockNo :: Tip blk -> BlockNo
tipBlockNo tip = withOrigin (BlockNo 0) identity (getTipBlockNo tip)

-- | Run a function of type `a -> m (Either e ())` over a list and return
-- the first `e` or `()`.
-- TODO: Is this not just `traverse` ?
traverseMEither :: Monad m => (a -> m (Either e ())) -> [a] -> m (Either e ())
traverseMEither action xs = do
  case xs of
    [] -> pure $ Right ()
    (y:ys) ->
      action y >>= either (pure . Left) (const $ traverseMEither action ys)


-- | Needed when debugging disappearing exceptions.
liftedLogException :: (MonadBaseControl IO m, MonadIO m) => Trace IO Text -> Text -> m a -> m a
liftedLogException tracer txt action =
    action `catch` logger
  where
    logger :: MonadIO m => SomeException -> m a
    logger e =
      liftIO $ do
        putStrLn $ "Caught exception: txt " ++ show e
        logError tracer $ txt <> textShow e
        throwIO e

-- | ouroboros-network catches 'SomeException' and if a 'nullTracer' is passed into that
-- code, the caught exception will not be logged. Therefore wrap all cardano-db-sync code that
-- is called from network with an exception logger so at least the exception will be
-- logged (instead of silently swallowed) and then rethrown.
logException :: Trace IO Text -> Text -> IO a -> IO a
logException tracer txt action =
    action `catch` logger
  where
    logger :: SomeException -> IO a
    logger e = do
      logError tracer $ txt <> textShow e
      throwIO e

-- | Eequired for testing or when disabling the metrics.
nullMetricSetters :: MetricSetters
nullMetricSetters =
  MetricSetters
    { metricsSetNodeBlockHeight = const $ pure ()
    , metricsSetDbQueueLength = const $ pure ()
    , metricsSetDbBlockHeight = const $ pure ()
    , metricsSetDbSlotHeight = const $ pure ()
    }

-- The network code catches all execptions and retries them, even exceptions generated by the
-- 'error' or 'panic' function. To actually force the termination of 'db-sync' we therefore
-- need a custom panic function that is guaranteed to abort when we want it to.
-- Hopefully, its obvious that this is mainly for debugging.
{-# NOINLINE panicAbort #-}
panicAbort :: Text -> a
panicAbort msg = do
  unsafePerformIO $ do
    Text.putStrLn msg
    exitImmediately (ExitFailure 1)
    panic msg -- To satisfy the type checker.

renderByteArray :: ByteArrayAccess bin => bin -> Text
renderByteArray =
  Text.decodeUtf8 . Base16.encode . Data.ByteArray.convert

renderPoint :: CardanoPoint -> Text
renderPoint point =
  case getPoint point of
    Origin -> "genesis"
    At blk -> mconcat
                [ "slot ", textShow (unSlotNo $ Point.blockPointSlot blk), ", hash "
                , renderByteArray $ toRawHash (Proxy @CardanoBlock) (Point.blockPointHash blk)
                ]
renderSlotList :: [SlotNo] -> Text
renderSlotList xs
  | length xs < 10 = textShow (map unSlotNo xs)
  | otherwise =
      mconcat [ "[", textShow (unSlotNo $ List.head xs), "..", textShow (unSlotNo $ List.last xs), "]" ]

maybeToStrict :: Maybe a -> Strict.Maybe a
maybeToStrict Nothing = Strict.Nothing
maybeToStrict (Just a) = Strict.Just a

whenJust :: Applicative m => Strict.Maybe a -> (a -> m ()) -> m ()
whenJust ma f = case ma of
  Strict.Nothing -> pure ()
  Strict.Just a -> f a

thrd3 :: (a, b, c) -> c
thrd3 (_, _, c) = c

