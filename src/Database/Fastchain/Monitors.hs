{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Monitors
  ( Monitors(..)
  , initMonitors
  ) where

import System.Remote.Monitoring
import System.Remote.Counter


data Monitors = Monitors
  { addRunTx :: Int -> IO ()
  , incPushTx :: IO ()
  , incAdviseTx :: IO ()
  , incMatureTx :: IO ()
  }


initMonitors :: Int -> IO Monitors
initMonitors port = do
  server <- forkServer "localhost" port
  let getInc name = inc <$> getCounter name server
      getAdd name = do c <- getCounter name server
                       pure $ add c . fromIntegral
  Monitors
    <$> getAdd "runTx"
    <*> getInc "pushTx"
    <*> getInc "adviseTx"
    <*> getInc "matureTx"
