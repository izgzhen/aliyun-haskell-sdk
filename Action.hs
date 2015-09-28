{-# LANGUAGE RankNTypes #-}

module Action where

import HTTP
import Config
import qualified Data.ByteString.Lazy as BL

class Action a where
    getQueryString :: a -> Config -> IO URL


callAPI :: (Action a) => Config -> a -> IO BL.ByteString
callAPI config action = do
    qs <- getQueryString action config
    httpsRequest qs

