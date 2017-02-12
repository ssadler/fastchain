{-# LANGUAGE OverloadedStrings #-}

module TestChainify where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Set as Set

import Database.Fastchain.Chain
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import TestCommon


testChainify :: TestTree
testChainify = testGroup "chainify" [
  testCase "getMatureTxs" $ do
    ct <- getCurrentTime
    let old = addUTCTime 3700 ct
        [a, b, c] = stx <$> [0,1,2]
        backlog = fromList [c, b, a]
    getMatureTxs old backlog @?= (fromList [c], [a, b])
  
  , testCase "noDoubleSpends" $ do
      let getSO txids = do
            txids @?= map mkTxid [1,2,3,4]
            pure $ Set.singleton $ mkTxid 1
      let tc = Chainify getSO undefined undefined undefined
          stxs = map stx [0,1,2] ++ map stx [1,2,3] ++ [(now, Tx "NNN" ["4"])]
      noDoubleSpends tc stxs >>= (@?= map stx [1,2,3])
  
  , testCase "integrate" $ do
      node <- mkTestNode
      let stxs = [(addUTCTime (-2) now, Tx "NNN" ["2"])] ++ map stx [0,1,2,3]
      modifyMVar_ (_backlog node) $ \_ -> pure $ fromList stxs
      let old = addUTCTime 7201 now
      processBacklog (ioChainify node) old
      txs <- db node selectTxsFrom 0
      txs @?= zip [1..] (head stxs : map stx [0,2])
  ]

