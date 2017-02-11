{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types


main :: IO ()
main = defaultMain $ testGroup "Tests" [
    testCase "testSetup" $ do
      node <- setup
      let ntxs c = query_ c "select count(*) from transactions"
      r <- db_ node ntxs
      r @?= [Only (3::Int)]

  , testCase "getSpentOf" $ do
      node <- setup
      txids <- db node getSpentOf ["a", "c"]
      txids @?= ["a"]
  ]



setup :: IO Node
setup = do
  let (p, s) = genKeyPairSeed 10000
      dsn = "dbname=fastchaintest"
  server <- newEmptyMVar
  peers <- newMVar mempty
  backlog <- newMVar mempty
  let node = Node dsn 5000 server p s peers backlog
      txs = [Tx "a" [], Tx "b" ["a"], Tx "c" ["b"]]
  db_ node createSchema
  db node insertTxs txs
  pure node
