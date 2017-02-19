{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Config where

import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Char

import System.Directory

import Database.Fastchain.Crypto
import Database.Fastchain.Prelude
import Database.Fastchain.Types


--------------------------------------------------------------------------------
-- Persistance


loadConfig :: FilePath -> IO Config
loadConfig configPath = do
  exists <- doesFileExist configPath
  if exists
     then do
       str <- BS.readFile configPath 
       pure $ either error id $ eitherDecode' $ fromStrict str
     else do
       let lastN n = reverse . take n . reverse
           s = lastN 3 $ filter isDigit $ configPath
       config <- genConfig s
       writeConfig configPath config
       pure config


writeConfig :: FilePath -> Config -> IO ()
writeConfig path = BS.writeFile path . toStrict . encodePretty


genConfig :: String -> IO Config
genConfig n = do
   keyPair <- genKeyPair
   pure $ Config { _keyPair = keyPair
                 , _peers = []
                 , _port = 51000 + read ('0':n)
                 , _dsn = "dbname=fastchain" ++ n
                 }


-- You have to import this yourself and run it, using ghci for example
--
linkConfigs :: [FilePath] -> IO ()
linkConfigs paths = do
  configs <- mapM loadConfig paths
  let peers = [(fst $ _keyPair c, _port c) | c <- configs]
  forM_ (zip configs paths) $ \(config,path) -> do
    writeConfig path $ config { _peers = peers }
