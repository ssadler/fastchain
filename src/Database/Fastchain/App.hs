{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Database.Fastchain.App where

import Control.Monad.Trans.Except

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import Database.PostgreSQL.Simple.Types


runTx :: Node -> STX -> ExceptT String IO ()
runTx node (t,tx) = do
  case _cmd tx of
       CreateApp sql _ -> createApp node (t,tx) sql
       CreateAsset _ -> createAsset node (t,tx) *> pure ()
       Call{} -> callApp node tx *> pure ()


createApp :: Node -> STX -> SQL -> ExceptT String IO ()
createApp node (t,tx) sql = do
  let op = db_ node $
        \c -> withTransaction c $ do
          _ <- query c "select create_app(?,?)" (toJSON tx, t) :: IO ([Only ()])
          execute_ c $ Query $ "set search_path to app_" <> encodeUtf8 (_txid tx)
          execute_ c $ Query $ encodeUtf8 sql
  lift $ (op *> pure ()) `catch` (print :: SqlError -> IO ())


createAsset :: Node -> STX -> ExceptT String IO Text
createAsset node (t,tx) = do
  lift $ db_ node $ \c -> do
    logE INFO "Creating asset"
    [Only assetId] <- query c "select create_asset(?,?)" (toJSON tx, t)
    logE INFO "Created asset"
    pure assetId


callApp :: Node -> Transaction -> ExceptT String IO ()
callApp node tx = do
  let call = _cmd tx
      proc = encodeUtf8 $ _proc call
      assetId = _asset call
      q = Query $ "select " <> proc <> "(?)"
  lift $ db_ node $ \c -> do
    setAssetPath c assetId
    query c q (Only $ toJSON tx) :: IO [Only ()]
    logE INFO (show tx)
  pure ()


getAppId :: Connection -> Text -> IO Text
getAppId c assetId = do
  [Only appId] <- query c "select app_id from assets where id = ?" (Only assetId)
  pure appId


setAssetPath :: Connection -> Text -> IO ()
setAssetPath c assetId = do
  appId <- encodeUtf8 <$> getAppId c assetId
  let sql = "set search_path to asset_" <> encodeUtf8 assetId <> ",app_" <> appId
  execute_ c $ Query sql
  pure ()


-- Lockdown
-- REVOKE ALL ON SCHEMA pg_catalog FROM PUBLIC ;
-- REVOKE ALL ON SCHEMA information_schema FROM PUBLIC ;
