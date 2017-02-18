{-# LANGUAGE OverloadedStrings #-}

module Database.Fastchain.Config where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL

import System.Directory

import Database.Fastchain.Crypto
import Database.Fastchain.Types


--------------------------------------------------------------------------------
-- Persistance


configPath :: FilePath
configPath = "config.json"


loadConfig :: IO Config
loadConfig = do
  exists <- doesFileExist configPath
  if exists
     then either error id . eitherDecode' <$> BL.readFile configPath 
     else do
       keyPair <- genKeyPair
       let config = Config keyPair mempty "dbname=fastchain"
       BL.writeFile configPath $ encode config
       pure config

