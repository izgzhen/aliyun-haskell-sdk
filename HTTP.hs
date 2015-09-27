module HTTP where

newtype URL = URL { unURL :: String } deriving (Eq, Show)

