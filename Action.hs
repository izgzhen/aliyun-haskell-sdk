module Action where

import HTTP
import Config

class Action a where
    getQueryString :: a -> Config -> IO URL
