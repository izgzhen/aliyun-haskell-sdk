-- Call Aliyun APIs in Haskell, experimental

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module API (
    getURL
) where

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
import Config

params time akid nouce action regionId = [
    ("AccessKeyId", unAkId akid),
    ("Action", action),
    ("Format", "xml"),
    ("RegionId", regionId),
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

getURL :: Config -> RegionId -> URL -> String -> IO URL
getURL config rid baseURL action = do
    let akid = _akId config
    let akSec = _akSec config
    time <- getTime
    nouce <- show <$> uuid
    let akSec' = Secret (unSecret akSec ++ "&")
    let table = params time akid nouce action (show rid)
    let sign = constructSign table
    let sig = base64 $ hmacSha1 akSec' sign
    return $ constructQuery baseURL (table ++ [("Signature", sig)])
 
 
