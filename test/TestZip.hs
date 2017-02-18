{-# LANGUAGE OverloadedStrings #-}

module TestZip where

import Test.Tasty
import Test.Tasty.HUnit

import Database.Fastchain.Prelude
import Database.Fastchain.Types
import Database.Fastchain.Zip

import TestCommon


zipTests :: TestTree
zipTests = testGroup "zip" [
    testCase "sortActionable" $ do
      let itx1 = mkItx 1
          itx2 = mkItx 2
--      sortActionable [QH (Just itx2), ()), (Nothing, ()), (Just itx1, ())]
--                 @?= [(Nothing, ()), (Just itx1, ()), (Just itx2, ())]
      pure ()

  , testCase "zipify" $ do
      testZipify [] @?= [R [], Delay 10000]
  ]

data ZipCall = R [QueueHead] | Delay Int
  deriving (Eq, Show)


testZipify :: [QueueHead] -> [ZipCall]
testZipify heads = 
  let ze = ZipEffects
              (error "a")
              (\n -> lift (modify (Delay n :)))
              (error "c")
              (\heads -> lift (modify (R heads :)))
   in snd $ testEffects (zipify heads) ze []
