{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts #-}

module Aliyun.OSS (
  Content
, packageData
, getObjects
, listObjects
, OSSHeader
, OSSResource(..)
, SubResource
, canonicalize
, putObject
, deleteObject
) where

import Aliyun.Auth
import Aliyun.Config
import Aliyun.HTTP

import Network.URI
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import Network.HTTP.Base as NB
import Network.HTTP.Headers
import qualified Network.HTTP as H
import Data.Char
import Data.List
import System.FilePath
import qualified Data.Map as M
import Text.Html (Html)
import qualified Data.Text as T

class Content a where
    _md5  :: a -> BC.ByteString
    _type :: a -> String
    _length :: a -> Int

instance Content String where
    _md5 s  = md5 (BC.pack s)
    _type s = "text/plain"
    _length s = BC.length $ BC.pack s

instance Content T.Text where
    _md5 s    = _md5 (T.unpack s)
    _type s   = "text/plain"
    _length s = BC.length $ BC.pack (T.unpack s)

instance Content Html where
    _md5 html = _md5 $ show html
    _type s = "text/html"
    _length s = BC.length $ BC.pack (show s)

instance Content BC.ByteString where
    _md5 s  = md5 s
    _type _ = "text/plain"
    _length s = BC.length s

constructSign :: Content a => RequestMethod -> a -> Config -> HTTPDate -> [OSSHeader] -> OSSResource -> String
constructSign verb content conf date ossHeaders resource =
    "OSS " ++ show (_akId conf) ++ ":" ++ base64 (hmacSha1 (_akSec conf) (Message $ BC.pack plain))
    where
        plain = show verb ++ "\n" ++
                (base64 . B.unpack $ _md5 content) ++ "\n" ++
                _type content ++ "\n" ++
                (BC.unpack (formatHTTPDate date)) ++ "\n" ++
                canonicalize ossHeaders ++
                canonicalize resource

constructSignEmpty :: RequestMethod -> Config -> HTTPDate -> [OSSHeader] -> OSSResource -> String
constructSignEmpty verb conf date ossHeaders resource =
    "OSS " ++ show (_akId conf) ++ ":" ++ base64 (hmacSha1 (_akSec conf) (Message $ BC.pack plain))
    where
        plain = show verb ++ "\n\n\n" ++
                (BC.unpack (formatHTTPDate date)) ++ "\n" ++
                canonicalize ossHeaders ++
                canonicalize resource

class Canonicalizable a where
    canonicalize :: a -> String


type OSSHeader = (String, String) -- Example ("X-OSS-Meta-Name", "Taobao")

data OSSResource = Bucket String (Maybe (String, SubResource))

type SubResource = [(String, Maybe String)] -- KVa

instance Canonicalizable [OSSHeader] where
    canonicalize = unlines . map p
                           . sortBy (\(k1, _) (k2, _) -> compare k1 k2)
                           . M.toList
                           . foldl (\hs h -> h `insertIn` hs) M.empty
                           . map (\(k, v) -> (map toLower k, v))
        where
            p :: (String, [String]) -> String
            p (k, vs) = k ++ ":" ++ (map (\c -> if c == ' ' then ',' else c) (unwords vs))

            insertIn :: (String, String) -> M.Map String [String] -> M.Map String [String]
            insertIn (k, v) hs = case M.lookup k hs of
                Just vs -> M.insert k (v:vs) hs
                Nothing -> M.insert k [v] hs

instance Canonicalizable OSSResource where
    canonicalize (Bucket bucketName maybeObj) = "/" ++ bucketName ++ "/" ++ p maybeObj
        where
            p Nothing = ""
            p (Just (objName, maybeSubResource)) = objName ++ p' maybeSubResource
            p' []     = ""
            p' (r:[]) = p'' r
            p' (r:subr) = "?" ++ foldl (\s r -> s ++ "&" ++ p'' r) (p'' r) subr
            p'' (k, Nothing) = k
            p'' (k, Just v)  = k ++ "=" ++ v

toHeader :: OSSHeader -> Header
toHeader (k, v) = mkHeader (HdrCustom k) v

packageData :: Content a => a -> RegionId -> Config -> RequestMethod -> [OSSHeader] -> OSSResource -> String -> IO (Request a)
packageData content region config verb ossheaders ossresource bucketName = do
    date <- getHTTPDate
    let s = (dropWhile (/='/') (tail $ canonicalize ossresource))
    let Just uri = parseURIReference s

    let (Bucket bucketName _) = ossresource

    let sign = constructSign verb content config date ossheaders ossresource

    let headers = [ mkHeader HdrAuthorization sign
                  , mkHeader HdrContentMD5 (base64 . B.unpack $ _md5 content)
                  , mkHeader HdrContentType $ _type content
                  , mkHeader HdrExpect "100-Continue"
                  , mkHeader HdrContentLength $ show (_length content)
                  , mkHeader HdrDate . BC.unpack $ formatHTTPDate date
                  , mkHeader HdrHost (bucketName ++ ".oss-" ++ show region ++ ".aliyuncs.com") ]  ++ map toHeader ossheaders


    -- let Just uri = parseURI ("http://" ++ bucketName ++ ".oss-" ++ show region ++ ".aliyuncs.com")

    return (Request uri verb headers content)


getObjects :: RegionId -> Config -> RequestMethod -> [OSSHeader] -> OSSResource -> String -> IO (Request BC.ByteString)
getObjects region config verb ossheaders ossresource bucketName = do
    date <- getHTTPDate
    let s = (dropWhile (/='/') (tail $ canonicalize ossresource))
    let Just uri = parseURIReference s

    print $ canonicalize ossresource

    let (Bucket bucketName _) = ossresource
    let sign = constructSignEmpty verb config date ossheaders ossresource
    let headers = [ mkHeader HdrAuthorization sign
                  -- , mkHeader HdrContentMD5 (base64 . B.unpack $ _md5 content)
                  -- , mkHeader HdrContentType $ _type content
                  -- , mkHeader HdrExpect "100-Continue"
                  -- , mkHeader HdrContentLength $ show (_length content)
                  , mkHeader HdrDate . BC.unpack $ formatHTTPDate date
                  , mkHeader HdrHost (bucketName ++ ".oss-" ++ show region ++ ".aliyuncs.com") ]  ++ map toHeader ossheaders

    -- let Just uri = parseURI ("http://" ++ bucketName ++ ".oss-" ++ show region ++ ".aliyuncs.com")

    return (Request uri verb headers "")



putObject obj config bucketName filename region = do
    let ossHeaders = []
    let ossresource = Bucket bucketName (Just (filename, []))
    req <- packageData obj region config NB.PUT ossHeaders ossresource  bucketName
    res <- Aliyun.HTTP.http req
    return (H.getResponseBody res)

deleteObject :: Config -> String -> String -> RegionId -> IO (BC.ByteString)
deleteObject config bucketName filename region = do
    let ossHeaders = []
    let ossresource = Bucket bucketName (Just (filename, []))
    req <- getObjects region config DELETE ossHeaders ossresource bucketName
    res <- Aliyun.HTTP.http req
    return $ BC.pack (show res)

listObjects :: Config -> String -> RegionId -> IO [String]
listObjects config bucketName region = do
    let ossHeaders = []
    let ossresource = Bucket bucketName Nothing
    req <- getObjects region config GET ossHeaders ossresource bucketName
    res <- Aliyun.HTTP.http req
    b <- H.getResponseBody res
    return $ (scanKeys (BC.unpack b))

scanKeys s = scanKeys' s False
  where
    keyStart = "Key>"
    scanKeys' "" _      = []
    scanKeys' ('<':s) _ = if take (length keyStart) s == keyStart
                            then scanKeys' (drop (length keyStart) s) True
                            else scanKeys' s False
    scanKeys' s True    = let (keyName, s') = span (/='<') s
                          in  keyName : scanKeys' (tail s) False
    scanKeys' s False   = scanKeys' (tail s) False

