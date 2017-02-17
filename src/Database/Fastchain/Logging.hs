module Database.Fastchain.Logging where


import Database.Fastchain.Prelude
import Database.Fastchain.Types

import Data.Text as T


--------------------------------------------------------------------------------
-- Logging functions

infoN :: Node -> Text -> IO ()
infoN node = 
  let name = "node"
   in infoM name . T.unpack
