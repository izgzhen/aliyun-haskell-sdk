module HTTP (
  URL(..)
, httpRequest
, httpsRequest
) where

import qualified Network.HTTP as H
import qualified Network.HTTP.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

newtype URL = URL { unURL :: String } deriving (Eq, Show)

httpsRequest  :: URL -> IO L.ByteString
httpsRequest (URL url) = C.simpleHttp url


httpRequest :: URL -> IO String
httpRequest (URL url) = H.simpleHTTP (H.getRequest url) >>= H.getResponseBody

