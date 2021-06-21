{-# LANGUAGE ScopedTypeVariables #-}
module Cardano.Db.Tool.Validate.TxAccounting
  ( validateTxAccounting
  ) where

import           Cardano.Db.Tool.Validate.Util

import           Cardano.Db

import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (left, runExceptT)
import           Control.Monad.Trans.Reader (ReaderT)

import           Data.Int (Int64)
import qualified Data.List as List

import           Database.Esqueleto (InnerJoin (..), Value (..),
                   entityVal, from, limit, on, orderBy, rand, select, unValue, val, where_, (==.), (>.), (^.))

import           Database.Persist.Sql (SqlBackend (..), toSqlKey)


validateTxAccounting :: IO ()
validateTxAccounting = do
    ids <- runDbNoLogging (queryTestTxIds testCount)
    putStrF $ "For " ++ show (length ids) ++ " transactions accounting is: "
    res <- runExceptT $ traverse validateAccounting ids
    case res of
      Left err -> error $ redText (reportError err)
      Right _ -> putStrLn $ greenText "ok"
  where
    testCount :: Int
    testCount = 100

-- -----------------------------------------------------------------------------

data ValidateError = ValidateError
  { veTxId :: !TxId
  , veFee :: !Ada
  , veDeposit :: !Int64
  , veWithdrawal :: !Ada
  , inputs :: ![TxOut]
  , outputs :: ![TxOut]
  }

reportError :: ValidateError -> String
reportError ve =
    mconcat
      [ "\nTxId: ", show (veTxId ve)
      , "\n  Fee: ", show (veFee ve)
      , case compare (veDeposit ve) 0 of
          LT -> "\n  Deposit: " ++ show (veDeposit ve)
          EQ -> mempty
          GT -> "\n  Refund: " ++ show (negate $ veDeposit ve)
      , if veWithdrawal ve == 0
          then mempty
          else "\n  Withdrawal: " ++ show (veWithdrawal ve)
      , "\n  TxIn: [", showTxOuts (inputs ve), "]"
      , "\n  TxOut: [", showTxOuts (outputs ve), "]"
      ]
  where
    showTxOuts :: [TxOut] -> String
    showTxOuts = List.intercalate "," . map showTxOut

    showTxOut :: TxOut -> String
    showTxOut txo =
      mconcat
        [ "TxId ", show (unTxId $ txOutTxId txo)
        , " Value ", show (word64ToAda . unDbLovelace $ txOutValue txo)
        ]

-- For a given TxId, validate the input/output accounting.
validateAccounting :: TxId -> ExceptT ValidateError IO ()
validateAccounting txId = do
    fee <- liftIO $ runDbNoLogging (queryTxFeeDeposit txId)
    let deposit = 0
    withdrawal <- liftIO $ runDbNoLogging (queryTxWithdrawal txId)
    ins <- liftIO $ runDbNoLogging (queryTxInputs txId)
    outs <- liftIO $ runDbNoLogging (queryTxOutputs txId)
    -- A refund is a negative deposit.
    when (deposit >= 0 && sumValues ins + withdrawal /= fee + adaDeposit deposit + sumValues outs) $
      left (ValidateError txId fee deposit withdrawal ins outs)
    when (deposit < 0 && sumValues ins + adaRefund deposit + withdrawal /= fee + sumValues outs) $
      left (ValidateError txId fee deposit withdrawal ins outs)
  where
    sumValues :: [TxOut] -> Ada
    sumValues txs = word64ToAda $ sum (map (unDbLovelace . txOutValue) txs)

    adaDeposit :: Int64 -> Ada
    adaDeposit = word64ToAda . fromIntegral

    adaRefund :: Int64 -> Ada
    adaRefund = word64ToAda . fromIntegral . negate

-- -------------------------------------------------------------------------------------------------

queryTestTxIds :: MonadIO m => Int -> ReaderT SqlBackend m ([TxId])
queryTestTxIds txCount = do
  -- Exclude all 'faked' generated TxId values from the genesis block (block_id == 1).
  tx <- select . from $ \ tx -> do
              where_ (tx ^. TxBlockId >. val (toSqlKey 1))
              orderBy [rand]
              limit (fromIntegral txCount)
              pure $ tx ^. TxId
  pure $ unValue <$> tx

queryTxFeeDeposit :: MonadIO m => TxId -> ReaderT SqlBackend m Ada
queryTxFeeDeposit txId = do
    res <- select . from $ \ tx -> do
              where_ (tx ^. TxId ==. val txId)
              pure (tx ^. TxFee)
    pure $ maybe 0 convert (listToMaybe res)
  where
    convert :: Value DbLovelace -> Ada
    convert (Value (DbLovelace w64)) = word64ToAda w64

queryTxInputs :: MonadIO m => TxId -> ReaderT SqlBackend m [TxOut]
queryTxInputs txId = do
  res <- select . from $ \ (tx `InnerJoin` txin `InnerJoin` txout) -> do
            on (txin ^. TxInTxOutId ==. txout ^. TxOutTxId)
            on (tx ^. TxId ==. txin ^. TxInTxInId)
            where_ (tx ^. TxId ==. val txId)
            where_ (txout ^. TxOutIndex ==. txin ^. TxInTxOutIndex)
            pure txout
  pure $ entityVal <$> res

queryTxOutputs :: MonadIO m => TxId -> ReaderT SqlBackend m [TxOut]
queryTxOutputs txId = do
  res <- select . from $ \ (tx `InnerJoin` txout) -> do
            on (tx ^. TxId ==. txout ^. TxOutTxId)
            where_ (tx ^. TxId ==. val txId)
            pure txout
  pure $ entityVal <$> res


queryTxWithdrawal :: MonadIO m => TxId -> ReaderT SqlBackend m Ada
queryTxWithdrawal txId = do
  res <- select . from $ \ withdraw -> do
            where_ (withdraw ^. WithdrawalTxId ==. val txId)
            pure (withdraw ^. WithdrawalAmount)
  -- It is probably not possible to have two withdrawals in a single Tx.
  -- If it is possible then there will be an accounting error.
  pure $ maybe 0 (word64ToAda . unDbLovelace . unValue) (listToMaybe res)
