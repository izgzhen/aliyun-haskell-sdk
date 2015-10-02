module Aliyun.HTTP (
  URL(..)
, httpRequest
, httpsRequest
, http
, https
, getHTTPDate
, HTTPDate(..)
, formatHTTPDate
) where

import Data.String (IsString(..))
import Network.HTTP.Date
import System.Posix.Time
import qualified Network.HTTP as H
import qualified Network.HTTP.Conduit as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Network.Connection (TLSSettings (..))

newtype URL = URL { unURL :: String } deriving (Eq, Show)

httpsRequest  :: URL -> IO L.ByteString
httpsRequest (URL url) = C.simpleHttp url


httpRequest :: URL -> IO String
httpRequest (URL url) = H.simpleHTTP (H.getRequest url) >>= H.getResponseBody

https request = do
    let settings = C.mkManagerSettings (TLSSettingsSimple True False False) Nothing 
    manager <- C.newManager settings
    res <- C.httpLbs request manager
    return res


getHTTPDate :: IO HTTPDate
getHTTPDate = epochTimeToHTTPDate <$> epochTime


instance IsString HTTPDate where
    fromString s = case parseHTTPDate (BC.pack s) of
                        Just d -> d
                        Nothing -> defaultHTTPDate


http req = H.simpleHTTP req
