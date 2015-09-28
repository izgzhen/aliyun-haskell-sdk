{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module OSS where

import qualified Data.ByteString.Char8 as BC
-- import qualified Data.ByteString as BS
import Auth
import Network.HTTP.Base (RequestMethod)

data Content = Content {
  _text :: BC.ByteString
, _type :: String
} deriving (Eq, Show)


constructSign :: RequestMethod -> Content -> Secret -> Int -> [OSSHeader] -> OSSResource -> String
constructSign verb content secret expires ossHeaders resource = base64 (hmacSha1 secret (Message $ BC.pack plain))
    where
        plain = show verb     ++ "\n" ++
                BC.unpack (md5 (_text content)) ++ "\n" ++
                _type content ++ "\n" ++
                show expires  ++ "\n" ++
                canonicalize ossHeaders ++
                canonicalize resource


class Canonicalizable a where
    canonicalize :: a -> String


data OSSHeader = X_OSS_Meta_Name

data OSSResource = OSSResource

instance Canonicalizable [OSSHeader] where
    canonicalize headers = "headers"

instance Canonicalizable OSSResource where
    canonicalize resource = "resource"