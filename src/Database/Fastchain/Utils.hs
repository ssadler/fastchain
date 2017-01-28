
module Database.Fastchain.Utils where

import Crypto.Hash
import Crypto.Error (CryptoFailable(..))
import Crypto.Random
import Crypto.PubKey.Ed25519

import Data.ByteArray as BA
import Data.ByteString as BS
import Data.ByteString.Base16 as B16

import Data.ByteString (ByteString)


sha3 :: BS.ByteString -> BS.ByteString
sha3 bs = B16.encode $ BS.pack $ BA.unpack (hash bs :: Digest SHA3_256)


genKeyPair :: IO (PublicKey, SecretKey)
genKeyPair = do
  drg <- getSystemDRG
  let (bs,_) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = secretKey (bs::ByteString)
  pure (toPublic sk, sk)


genKeyPairSeed :: Int -> (PublicKey, SecretKey)
genKeyPairSeed n =
  let drg = drgNewSeed $ seedFromInteger $ fromIntegral n
      (bs,_) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = secretKey (bs::ByteString)
   in (toPublic sk, sk)

