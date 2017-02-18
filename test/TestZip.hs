{-# LANGUAGE OverloadedStrings #-}

module TestZip where

import Test.Tasty
import Test.Tasty.HUnit

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types
import Database.Fastchain.Zip

import TestCommon


zipTests :: TestTree
zipTests = testGroup "zip" [
    testCase "sortActionable" $ do
      ch <- newChan
      let itx1 = mkItx 1
          itx2 = mkItx 2
          (pk,_) = genKeyPairSeed 0
          [q1,q2,q3] = [QH m pk ch | m <- [Just itx2, Nothing, Just itx1]]
      sortActionable [q1,q2,q3] @?= [q2,q3,q1]

  , testCase "zipifyDelay" $ do
      let qh = (mkQh 0) {_mitx=Nothing}
      testZipify [qh] @?= [Delay, UpdateHead qh, R [qh]]

  , testCase "zipifyOne" $ do
      let qh0@(QH (Just (_,sig)) pk _) = mkQh 0
      testZipify [qh0] @?= [ Yield (mkStx 0,[(pk,sig)])
                           , UpdateHead qh0
                           , R [qh0]
                           ]

  , testCase "zipifySome" $ do
      let qh0@(QH (Just (_,sig)) pk _) = mkQh 0
          qh1 = mkQh (-1)
      testZipify [qh0,qh0,qh1] @?= [ Yield (mkStx 0,[(pk,sig),(pk,sig)])
                                   , UpdateHead qh0
                                   , UpdateHead qh0
                                   , R [qh1, qh0, qh0]
                                   ]
  ]


mkQh :: Int -> QueueHead
mkQh n =
  let (pk,_) = genKeyPairSeed n
      itx = mkItx n
   in QH (Just itx) pk (unsafePerformIO newChan)


data ZipCall = R [QueueHead] | Delay | UpdateHead QueueHead | Yield (STX, SigMap)
  deriving (Eq, Show)


testZipify :: [QueueHead] -> [ZipCall]
testZipify heads = 
  let y c v = lift $ modify $ (c v :)
      ze = ZipEffects
              (y Yield)
              (lift $ modify (Delay:))
              (\qh -> y UpdateHead qh *> pure qh)
              (y R)
   in reverse $ snd $ testEffects (zipify heads) ze []
