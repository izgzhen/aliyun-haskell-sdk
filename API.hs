-- Call Aliyun APIs in Haskell, experimental

{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module API (
  getURL
, getRegion
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

contantParams =
    [("Format", "xml"),
     ("SignatureMethod", "HMAC-SHA1"),
     ("SignatureVersion", "1.0")]

getURL :: Config -> RegionId -> URL -> String -> String -> [(String, String)] -> IO URL
getURL config rid baseURL action version params = do
    let akid = _akId config
    let akSec = _akSec config
    time <- getTime
    nouce <- show <$> uuid
    let akSec' = Secret (unSecret akSec ++ "&")
    let params' = params ++ [ ("RegionId", show rid)
                            , ("Action", action)
                            , ("SignatureNonce", nouce)
                            , ("Timestamp", time)
                            , ("AccessKeyId", unAkId akid)
                            , ("Version", version) ]
                         ++ contantParams
    let sign = constructSign params'
    let sig = base64 $ hmacSha1 akSec' sign
    return $ constructQuery baseURL (params' ++ [("Signature", sig)])


-- A Hacky Parser
getRegion config =  Prelude.head . _regionId . Prelude.head $  _ecses config

