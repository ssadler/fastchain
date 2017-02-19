{-# LANGUAGE OverloadedStrings #-}

module TestCommon
  ( module ALL
  , now
  , mkTxid
  , mkStx
  , mkItx
  , mkTestNode
  , testEffects
  , unsafePerformIO
  ) where

import Control.Monad.Trans.State as ALL

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Schema
import Database.Fastchain.Types

import System.IO.Unsafe


--------------------------------------------------------------------------------
-- Test Effects


testEffects :: Effectful ef (State s) a -> ef (State s) -> s -> (a, s)
testEffects act effects = runState $ runEffects act effects


--------------------------------------------------------------------------------
--


now :: UTCTime
now = unsafePerformIO getCurrentTime


mkTxid :: Int -> Txid
mkTxid = Txid . c8Pack . show


mkStx :: Int -> STX
mkStx n =
  let t = addUTCTime (realToFrac $ n * 3600) now
      tx = Tx (mkTxid n) [mkTxid $ n+1]
   in (t, tx)


mkItx :: Int -> ITX
mkItx n =
  let kp = genKeyPairSeed n
      stx@(_, Tx (Txid txid) _) = mkStx n
   in (stx,sign kp txid)


mkTestNode :: IO Node
mkTestNode = do
  let keyPair = genKeyPairSeed 10000
      dsn = "dbname=fastchaintest"
      conf = Config keyPair [] 50001 dsn
  node <- Node conf <$> newEmptyMVar <*> newMVar mempty
  db_ node createSchema
  pure node
