{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent
import qualified Data.Set as Set

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import TestCommon


main :: IO ()
main = defaultMain $ testGroup "Tests" [ dbTests
                                       ]


--------------------------------------------------------------------------------
-- dbTests
--

dbTests :: TestTree
dbTests = testGroup "dbTests" [
    testCase "testSetup" $ do
      node <- dbSetup 2
      let ntxs c = query_ c "select count(*) from transactions"
      r <- db_ node ntxs
      r @?= [Only (2::Int)]

  , testCase "getSpentOf" $ do
      node <- dbSetup 100
      txids <- db node getSpentOf [mkTxid 3, mkTxid 100001]
      txids @?= Set.singleton (mkTxid 3)
  ]


dbSetup :: Int -> IO Node
dbSetup ntxs = do
  node <- mkTestNode
  db node insertTxs $ stx <$> [0..ntxs-1]
  pure node
