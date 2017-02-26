{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Database.Fastchain.Http where

import Database.Fastchain.Prelude
import Database.Fastchain.Node
import Database.Fastchain.Types

import Network.HTTP.Types.Status

import Web.Scotty hiding (request)


runHttp :: Node -> IO ()
runHttp node = scotty 8500 $ do
  get "/transactions/" $ do
    json $ jsonTxsFrom .% (True,False)

  post "/transactions/" $ do
    tx <- jsonData
    lift $ pushTx node tx
    status accepted202
    json $ "{status}" .% ("accepted"::Text)


jsonTxsFrom :: Structure
jsonTxsFrom = "{next,transactions}"


nextSeq :: [(Integer, a)] -> Maybe Integer
nextSeq [] = Nothing
nextSeq txs = let (s, _) = last txs in Just $ s + 1
