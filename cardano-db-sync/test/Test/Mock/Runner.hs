
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Test.Mock.Runner
  ( ChainAction (..)
  , runChainSyncTest
  ) where

import           Ouroboros.Network.Block (Point, Tip)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined (ChainSyncClientPipelined)
import           Test.Mock.LocalChainSync


runChainSyncTest :: [ChainAction blk] -> IO a
runChainSyncTest chainActions = playChainFragment chainActions dbSyncChainClient



dbSyncChainClient :: ChainSyncClientPipelined blk (Point blk) (Tip blk) IO a
