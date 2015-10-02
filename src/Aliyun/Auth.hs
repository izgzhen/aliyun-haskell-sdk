module Aliyun.Auth (
  hmacSha1
, base64
, md5
, Secret(..)
, Message(..)
, AkId(..)
) where

import Data.ByteString.Base64 as BB
import Data.ByteString.Char8 as BC
import Data.ByteString as B
import Data.HMAC
import Data.Word (Word8)
import qualified Data.Digest.MD5 as M


newtype Secret  = Secret  { unSecret  :: String } deriving (Eq)
instance Show Secret where
    show (Secret s) = s

newtype Message = Message { unMessage :: ByteString } deriving (Eq)
instance Show Message where
    show (Message m) = BC.unpack m

hmacSha1 :: Secret -> Message -> [Word8]
hmacSha1 secret msg = hmac_sha1 (B.unpack . BC.pack $ unSecret secret) (B.unpack $ unMessage msg)

base64 :: [Word8] -> String
base64 = BC.unpack . BB.encode . B.pack

newtype AkId = AkId { unAkId :: String } deriving (Eq)

instance Show AkId where
    show (AkId id) = id

md5 :: B.ByteString -> B.ByteString
md5 = B.pack . M.hash . B.unpack

