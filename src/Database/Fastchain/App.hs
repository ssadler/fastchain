{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

module Database.Fastchain.App where

import Control.Monad.Trans.Except

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types
import Database.Fastchain.Monitors

import Database.PostgreSQL.Simple.Types


runApp :: Node -> IO ()
runApp node = do
  let chan = _exec node
  let batch txs = do
        mtx <- if null txs
           then Just <$> readChan chan
           else do
             empty <- isEmptyChan chan
             if empty then pure Nothing
                      else Just <$> readChan chan
        case mtx of
            Nothing -> do
              when (not $ null txs) $ do
                runTxs node txs
                addRunTx (_monitors node) $ length txs
              batch []
            Just tx -> batch (tx:txs)
  batch []


runTxs :: Node -> [STX] -> IO (Either String ())
runTxs node txs = runExceptT $ do
  writeChain node txs
  mapM (runTx node) txs
  pure () 


runTx :: Node -> STX -> ExceptT String IO ()
runTx node (t,tx) = do
  case _cmd tx of
       CreateApp sql _ -> createApp node (t,tx) sql
       Call{} -> callApp node tx *> pure ()
       Noop -> pure ()


writeChain :: Node -> [STX] -> ExceptT String IO Int64
writeChain node stxs = lift $ do
  let vals = [(_txid tx, toJSON tx, t) | (t,tx) <- stxs]
      sql = "insert into transactions (txid, payload, ts) values (?, ?, ?)"
  db_ node $ \c -> executeMany c sql vals


createApp :: Node -> STX -> SQL -> ExceptT String IO ()
createApp node (t,tx) sql = do
  let op = db_ node $
        \c -> withTransaction c $ do
          _ <- query c "select create_app(?,?)" (toJSON tx, t) :: IO ([Only ()])
          execute_ c $ Query $ "set search_path to app_" <> _txid tx
          execute_ c $ Query sql
  lift $ (op *> pure ()) `catch` (print :: SqlError -> IO ())


callApp :: Node -> Transaction -> ExceptT String IO ()
callApp node tx = do
  let call = _cmd tx
      proc = encodeUtf8 $ _proc call
      q = Query $ "select " <> proc <> "(?)"
  lift $ db_ node $ \c -> do
    setPath c $ _app call
    query c q (Only $ toJSON tx) :: IO [Only ()]
    logE INFO (show tx)
  pure ()


setPath :: Connection -> ByteString -> IO ()
setPath c appId = do
  execute_ c $ Query $ "set search_path to app_" <> appId
  pure ()


-- Lockdown
-- REVOKE ALL ON SCHEMA pg_catalog FROM PUBLIC ;
-- REVOKE ALL ON SCHEMA information_schema FROM PUBLIC ;
