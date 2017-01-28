{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Http where

import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import Database.PostgreSQL.Simple (Connection)
import Web.Scotty


runServer :: Connection -> Int -> IO ()
runServer conn port = scotty port $ do
  get "/transactions/" $ do
    offset <- param "offset"
    txs <- lift $ selectTxsFrom conn offset
    json $ jsonTxsFrom .% (nextSeq txs, [tx | (_:.tx) <- txs])

  post "/transactions/" $ do
    tx <- (.! "{tx}") <$> jsonData
    lift $ insertTx conn tx
    json $ "{status}" .% ("ok"::Text)


jsonTxsFrom :: Structure
jsonTxsFrom = "{next,transactions}"


nextSeq :: [(Only Integer :. Transaction)] -> Maybe Integer
nextSeq [] = Nothing
nextSeq txs = let (Only s :. _) = last txs in Just $ s + 1
