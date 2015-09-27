-- Call Aliyun APIs in Haskell, experimental

module API where
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import Network.HTTP
import Data.Time.Format
import Data.Time.Clock
import Data.ByteString.Char8 as BC
import Data.ByteString as B
import Network.HTTP.Base
import System.UUID.V4
import Data.String.Utils
import Data.List (sortBy)

import Auth
import HTTP

params time akid nouce = [
    ("AccessKeyId", unAkId akid),
    ("Action", "DescribeRegions"),
    ("Format", "xml"),
    ("RegionId", "cn-shenzhen"),
    ("SignatureMethod", "HMAC-SHA1"),
    ("SignatureNonce", nouce),
    ("SignatureVersion", "1.0"),
    ("Timestamp", time),
    ("Version", "2014-05-26")
    ]

sortTable = sortBy (\(k, _) (k', _) -> compare k k')

replaceEq :: String -> String
replaceEq = replace "=" "%3D" . replace "&" "%26" . replace "%3A" "%253A"

constructSign :: [(String, String)] -> Message
constructSign table = Message $ BC.pack ("GET&" ++ urlEncode "/" ++ "&" ++ replaceEq (urlEncodeVars (sortTable table)))

constructQuery :: URL -> [(String, String)] -> URL
constructQuery baseURL table = URL (unURL baseURL ++ "?" ++ urlEncodeVars table)

getTime :: IO String
getTime = do
    utcTime <- getCurrentTime
    let format = iso8601DateFormat (Just "%H:%M:%SZ")
    let time = formatTime defaultTimeLocale format utcTime
    return time

getURL :: URL -> AkId -> Secret -> IO URL
getURL baseURL akid aksec = do
    time <- getTime
    nouce <- show <$> uuid
    let aksec' = Secret (unSecret aksec ++ "&")
    let sign = constructSign (params time akid nouce)
    let sig = base64 $ hmacSha1 aksec' sign
    return $ constructQuery baseURL (params time akid nouce ++ [("Signature", sig)])
 
 
