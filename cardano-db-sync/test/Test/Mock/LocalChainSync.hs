
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Test.Mock.LocalChainSync
  ( ChainAction (..)
  , chainSyncServerExample
  , playChainFragment
  ) where

import           Data.List (nub)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTimer

import           Cardano.Slotting.Slot

import           Network.TypedProtocol.Proofs (connect)

import           Ouroboros.Network.Block (HasHeader (..), HeaderHash, Tip (..),
                   castPoint, castTip, genesisPoint)
import qualified Ouroboros.Network.Block as Block
import           Ouroboros.Network.MockChain.Chain (Chain (..))
-- import           Ouroboros.Network.Point (WithOrigin (..))
import           Ouroboros.Network.MockChain.Chain (ChainUpdate (..), Point (..))
import qualified Ouroboros.Network.MockChain.Chain as Chain
import           Ouroboros.Network.MockChain.ProducerState (ChainProducerState (..),
                   FollowerId)
import qualified Ouroboros.Network.MockChain.ProducerState as ChainProducerState
import           Ouroboros.Network.Protocol.ChainSync.Client (ChainSyncClient,
                   chainSyncClientPeer)
import           Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer (..),
                   ServerStIdle (..), ServerStIntersect (..), ServerStNext (..),
                   chainSyncServerPeer)

import           Test.QuickCheck
import qualified Debug.Trace as Debug


data Config blk = Config {
    cfgChain        :: [blk]
    -- ^ a pre-generated list of blocks from which the server will take blocks
    -- and apply them to its chain after applying 'cfgFixuBlock'.  This list is
    -- consumed from left to right (oldest to newest).

  , cfgFixupBlock   :: WithOrigin blk
                    -- ^ previous block, or the origin
                    -> blk
                    -- ^ header to fix
                    -> blk
    -- ^ fix a block so it applies onto the previous one.  This gives a way to
    -- preserve a chain invariant.

  , cfgSlotDuration :: DiffTime
    -- ^ slot length, in seconds
  }


data ChainAction
  = RollForward
  | RollBackward !SlotNo
  -- ^ we need 'Point' to make rollbacks, but it would be more difficult to
  -- generate them.
  --
  -- Note: the generator does not make sure that the slot has a block, and this
  -- is required to make a successful rollback.  There ought to be a test which
  -- validates this property for generated data.
  deriving (Eq, Show)


genChainAction :: Gen ChainAction
genChainAction =
    frequency [ (3, pure RollForward)
              , (1, RollBackward . SlotNo <$> arbitrary)
              ]


-- | A stateful shrinker for a 'ChainAction'.   The 'SlotNo' is the current
-- 'SlotNo'.
--
shrinkChainAction :: SlotNo -> ChainAction -> [ChainAction]
shrinkChainAction _            RollForward = []
shrinkChainAction currentSlot (RollBackward slot) =
      Debug.traceShow (slot, currentSlot) $
      if succ slot < currentSlot
        -- make the fork shallower
        then nub
           $ [ RollBackward (succ slot)
             , RollBackward $ SlotNo ( unSlotNo slot
                                     + unSlotNo (currentSlot - slot) `div` 2
                                     )
             ]
        else [RollForward]


-- | A list of 'ChainActions'.  Unlike a 'Chain' list of chain actions is more
-- naturally a right associative list, e.g. actions are applied from left to
-- right.
--
newtype ChainActions = ChainActions [ChainAction]
  deriving Show


-- | Annotate list of 'ChainAction' with slot numbers (before the transition).
--
withSlots :: [ChainAction] -> [(SlotNo, ChainAction)]
withSlots = go (SlotNo 0)
  where
    go :: SlotNo -> [ChainAction] -> [(SlotNo, ChainAction)]
    go !_slotNo [] = []
    go !slotNo (a@RollForward : as) =
      (slotNo, a) : go (succ slotNo) as
    go !slotNo  (a@(RollBackward slot) : as) =
      (slotNo, a) : go slot as


fixupChainActions :: [ChainAction] -> [ChainAction]
fixupChainActions = fixupRollbacks (SlotNo 0) . fixupHead
  where
    -- list of chain actions must either be empty or start with 'RollForward'
    fixupHead :: [ChainAction] -> [ChainAction]
    fixupHead [] = []
    fixupHead as@(RollForward    :  _) = as
    fixupHead    (RollBackward _ : as) = fixupHead as

    -- rollbacks cannot go passed genesis;  Ouroboros further restricts how deep
    -- they can be.
    fixupRollbacks :: SlotNo
       -- ^ current slot
       -> [ChainAction]
       -> [ChainAction]
    fixupRollbacks _currSlot [] = []

    fixupRollbacks !currSlot (a@RollForward : as) =
        a : fixupRollbacks (succ currSlot) as

    fixupRollbacks !currSlot (RollBackward slot : as) =
        RollBackward slot' : fixupRollbacks slot' as
      where
        slot' = slot `min` pred currSlot


instance Arbitrary ChainActions where
    arbitrary = ChainActions
              . fixupChainActions
            <$> listOf genChainAction

    -- todo: this shrink function might produce the same result twice.
    shrink (ChainActions as) =
          map (ChainActions . fixupChainActions . map snd)
        . shrinkList
            (\(slot, action) ->
              [ (slot, action')
              | action' <- shrinkChainAction slot action
              ])
        . withSlots
        $ as
  


playChainFragment :: forall blk m a.
                     ( MonadAsync m
                     , MonadDelay m
                     , HasHeader blk
                     )
                  => Config blk
                  -> ChainActions
                  -> ChainSyncClient blk (Point blk) (Tip blk) m a
                  -> m a
playChainFragment Config { cfgChain, cfgFixupBlock, cfgSlotDuration }
                 (ChainActions chainActions0)
                 chainSyncClient = do
    cpsvar <- newTVarIO (ChainProducerState.initChainProducerState Genesis)
    snd <$>
      serverDriver cpsvar chainActions0 cfgChain
      `concurrently`
      ((\(a, _, _) -> a) <$>
        -- run the client against server without any network in between,
        -- like a consumer stream against a producer.
        chainSyncClientPeer chainSyncClient
        `connect`
        chainSyncServerPeer (chainSyncServerExample () cpsvar))
  where
    serverDriver :: StrictTVar m (ChainProducerState blk)
                 -> [ChainAction]
                 -> [blk]
                 -> m ()
    serverDriver _cpsvar [] _chain = return ()

    serverDriver  cpsvar (RollForward : chainActions) (block : blocks) = do
      atomically $ do
        cps@ChainProducerState { chainState } <- readTVar cpsvar
        case chainState of
          Genesis ->
            writeTVar cpsvar (ChainProducerState.addBlock
                               (cfgFixupBlock Origin block) cps)
          _ :> blk ->
            writeTVar cpsvar (ChainProducerState.addBlock
                               (cfgFixupBlock (At blk) block) cps)
      threadDelay cfgSlotDuration
      serverDriver cpsvar chainActions blocks

    serverDriver cpsvar (RollForward : chainActions) [] =
      serverDriver cpsvar chainActions []

    serverDriver  cpsvar (RollBackward slot : chainActions) blocks = do
      atomically $ do
        cps <- readTVar cpsvar
        -- we need 'Point' to apply the rollback, find it on the chain
        case Chain.findBlock (\blk -> Block.blockSlot blk == slot)
                             (ChainProducerState.chainState cps) of
          -- TODO: a better error; it should be guaranteed by the generated data
          -- that this does not happen.
          Nothing  -> error "serverDriver: slot not found in the chain"
          Just blk ->
            case ChainProducerState.rollback (Chain.blockPoint blk :: Point blk) cps of
              Just cps' -> writeTVar cpsvar cps'
              Nothing   -> error "serverDriver: impossible happened"
      threadDelay cfgSlotDuration
      serverDriver cpsvar chainActions blocks



chainSyncServerExample
    :: forall blk header m a.
        (HasHeader header , MonadSTM m , HeaderHash header ~ HeaderHash blk)
    => a -> StrictTVar m (ChainProducerState header)
    -> ChainSyncServer header (Point blk) (Tip blk) m a
chainSyncServerExample recvDoneClient chainvar =
    ChainSyncServer $ idle <$> newFollower
  where
    idle :: FollowerId -> ServerStIdle header (Point blk) (Tip blk) m a
    idle fid =
      ServerStIdle
        { recvMsgRequestNext = handleRequestNext fid
        , recvMsgFindIntersect = handleFindIntersect fid
        , recvMsgDoneClient = pure recvDoneClient
        }

    idleNext :: FollowerId -> ChainSyncServer header (Point blk) (Tip blk) m a
    idleNext = ChainSyncServer . pure . idle

    handleRequestNext
        :: FollowerId
        -> m (Either (ServerStNext header (Point blk) (Tip blk) m a) (m (ServerStNext header (Point blk) (Tip blk) m a)))
    handleRequestNext r = do
      mupdate <- tryReadChainUpdate r
      case mupdate of
        Just update -> pure (Left (sendNext r update))
        Nothing     -> pure (Right (sendNext r <$> readChainUpdate r))
                       -- Follower is at the head, have to block and wait for
                       -- the producer's state to change.

    sendNext
        :: FollowerId -> (Tip blk, ChainUpdate header header)
        -> ServerStNext header (Point blk) (Tip blk) m a
    sendNext r (tip, AddBlock b) = SendMsgRollForward  b tip (idleNext r)
    sendNext r (tip, RollBack p) = SendMsgRollBackward (castPoint p) tip (idleNext r)

    handleFindIntersect
        :: FollowerId -> [Point blk]
        -> m (ServerStIntersect header (Point blk) (Tip blk) m a)
    handleFindIntersect r points = do
      -- TODO: guard number of points
      -- Find the first point that is on our chain
      changed <- improveReadPoint r points
      case changed of
        (Just pt, tip) -> pure $ SendMsgIntersectFound pt tip (idleNext r)
        (Nothing, tip) -> pure $ SendMsgIntersectNotFound tip (idleNext r)

    newFollower :: m FollowerId
    newFollower =
      atomically $ do
        cps <- readTVar chainvar
        let (cps', rid) = ChainProducerState.initFollower genesisPoint cps
        writeTVar chainvar cps'
        pure rid

    improveReadPoint :: FollowerId -> [Point blk] -> m (Maybe (Point blk), Tip blk)
    improveReadPoint rid points =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.findFirstPoint (map castPoint points) cps of
          Nothing -> let chain = ChainProducerState.chainState cps in
                        pure (Nothing, castTip (Chain.headTip chain))
          Just ipoint -> do
            let !cps' = ChainProducerState.updateFollower rid ipoint cps
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            pure (Just (castPoint ipoint), castTip (Chain.headTip chain))

    tryReadChainUpdate :: FollowerId -> m (Maybe (Tip blk, ChainUpdate header header))
    tryReadChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.followerInstruction rid cps of
          Nothing -> pure Nothing
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            pure $ Just (castTip (Chain.headTip chain), u)

    readChainUpdate :: FollowerId -> m (Tip blk, ChainUpdate header header)
    readChainUpdate rid =
      atomically $ do
        cps <- readTVar chainvar
        case ChainProducerState.followerInstruction rid cps of
          Nothing -> retry
          Just (u, cps') -> do
            writeTVar chainvar cps'
            let chain = ChainProducerState.chainState cps'
            pure (castTip (Chain.headTip chain), u)
