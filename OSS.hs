{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts #-}

module OSS (
  Content(..)
, constructSign
, OSSHeader
, OSSResource(..)
, SubResource
, getHTTPDate
) where

import Auth
import Config

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Base
import Network.HTTP.Date
import Data.Char
import Data.List
import System.FilePath
import qualified Data.Map as M
import System.Posix.Time
import Data.String (IsString(..))

data Content = Content {
  _text :: BC.ByteString
, _type :: String
} deriving (Eq, Show)


constructSign :: RequestMethod -> Content -> Config -> HTTPDate -> [OSSHeader] -> OSSResource -> String
constructSign verb content conf date ossHeaders resource =
    base64 (hmacSha1 (_akSec conf) (Message $ BC.pack plain))
    where
        plain = show verb     ++ "\n" ++
                BC.unpack (md5 (_text content)) ++ "\n" ++
                _type content ++ "\n" ++
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
    canonicalize (Bucket bucketName maybeObj) = "/" ++ bucketName </> p maybeObj
        where
            p Nothing = ""
            p (Just (objName, maybeSubResource)) = objName ++ p' maybeSubResource
            p' []     = ""
            p' (r:[]) = p'' r
            p' (r:subr) = "?" ++ foldl (\s r -> s ++ "&" ++ p'' r) (p'' r) subr
            p'' (k, Nothing) = k
            p'' (k, Just v)  = k ++ "=" ++ v

getHTTPDate :: IO HTTPDate
getHTTPDate = epochTimeToHTTPDate <$> epochTime



instance IsString HTTPDate where
    fromString s = case parseHTTPDate (BC.pack s) of
                        Just d -> d
                        Nothing -> defaultHTTPDate

