{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS

import Test.Tasty
import Test.Tasty.HUnit

import Control.Concurrent

import Database.Fastchain.Chain
import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types


main :: IO ()
main = defaultMain $ testGroup "Tests" [ dbTests
                                       , pureTests
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
      txids @?= [mkTxid 3]
  ]


dbSetup :: Int -> IO Node
dbSetup ntxs = do
  let (p, s) = genKeyPairSeed 10000
      dsn = "dbname=fastchaintest"
  server <- newEmptyMVar
  peers <- newMVar mempty
  backlog <- newMVar mempty
  let node = Node dsn 5000 server p s peers backlog
  db_ node createSchema
  db node insertTxs $ mkTxs ntxs
  pure node


mkTxs :: Int -> [Transaction]
mkTxs 0 = []
mkTxs n = Tx (mkTxid n) [mkTxid $ n+1] : mkTxs (n-1)


mkTxid :: Int -> Txid
mkTxid = decodeUtf8 . sha3 . BS.pack . show


--------------------------------------------------------------------------------
-- pure


pureTests :: TestTree
pureTests = testGroup "pureTests" [
  testCase "getMatureTxs" $ do
    ct <- getCurrentTime
    let old = addUTCTime (-3700) ct
        [a, b, c] = zip (steppedTimes ct (-3600)) (mkTxs 3)
        backlog = fromList [c, b, a]
    getMatureTxs old backlog @?= (fromList [a, b], [c])
  ]


steppedTimes :: UTCTime -> Double -> [UTCTime]
steppedTimes t n = t : steppedTimes (addUTCTime (realToFrac n) t) n

