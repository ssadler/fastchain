{-# LANGUAGE OverloadedStrings #-}

module TestChain2 where

import Test.Tasty
import Test.Tasty.HUnit

import Database.Fastchain.Chain2
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import TestCommon


testChain2 :: TestTree
testChain2 = testGroup "chain2" [
  testCase "cmpActionable" $ do
  ]

