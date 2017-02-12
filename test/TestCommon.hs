{-# LANGUAGE OverloadedStrings #-}

module TestCommon where

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import qualified Data.Text as T

import System.IO.Unsafe


mkTxid :: Int -> Txid
mkTxid = T.pack . show
  

now :: UTCTime
now = unsafePerformIO getCurrentTime


stx :: Int -> STX
stx n = (addUTCTime (realToFrac $ n * 3600) now,
         Tx (mkTxid n) [mkTxid $ n+1])


mkTestNode :: IO Node
mkTestNode = do
  let (p, s) = genKeyPairSeed 10000
      dsn = "dbname=fastchaintest"
  server <- newEmptyMVar
  peers <- newMVar mempty
  backlog <- newMVar mempty
  let node = Node dsn 5000 server p s peers backlog
  db_ node createSchema
  pure node
