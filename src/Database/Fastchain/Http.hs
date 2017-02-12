{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Http where

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import Database.PostgreSQL.Simple (Connection)
import Web.Scotty hiding (request)


runHttp :: Node -> IO ()
runHttp node = do
  conn <- connectPostgreSQL (_dsn node)
  runServer conn (_server node) (_httpPort node)


runServer :: Connection -> Server -> Int -> IO ()
runServer conn server = flip scotty $ do
  get "/transactions/" $ do
    offset <- param "offset"
    txs <- lift $ selectTxsFrom conn offset
    json $ jsonTxsFrom .% (nextSeq txs, [tx | (_, tx) <- txs])

  post "/transactions/" $ do
    tx <- (.! "{tx}") <$> jsonData
    lift $ push server $ PostTx tx
    json $ "{status}" .% ("ok"::Text)


jsonTxsFrom :: Structure
jsonTxsFrom = "{next,transactions}"


nextSeq :: [(Integer, a)] -> Maybe Integer
nextSeq [] = Nothing
nextSeq txs = let (s, _) = last txs in Just $ s + 1
