-- Management user config file
-- Work with AliyunCLI's dotfiles

module Aliyun.Config where
import Aliyun.Auth
import Language.Haskell.TH
import System.FilePath
import Data.List

data Config = Config {
    _akId  :: AkId,
    _akSec :: Secret,
    _ecses :: [ECSConfig]
} deriving (Show)

data ECSConfig = ECSConfig {
    _regionId :: [RegionId]
} deriving (Show)

data RegionId = CnShenZhen
              | CnHongKong

instance Show RegionId where
    show CnShenZhen = "cn-shenzhen"
    show CnHongKong = "cn-hongkong"

readConfig :: FilePath -> IO (Maybe Config)
readConfig path = do
    cred <- readFile (path </> "credentials")
    conf <- readFile (path </> "configure")
    return $ do
        (akId, akSec) <- parseCred cred
        ecses <- parseConf conf
        return $ Config akId akSec ecses

parseCred :: String -> Maybe (AkId, Secret)
parseCred s = do
    let findStartWith prefix = find (\l -> (head $ words l) == prefix) (lines s)
    akIdLn  <- findStartWith "aliyun_access_key_id"
    akSecLn <- findStartWith "aliyun_access_key_secret"
    akId  <- (words akIdLn)  `at` 2
    akSec <- (words akSecLn) `at` 2
    return (AkId akId, Secret akSec)


at xs i = if length xs > i then Just (xs !! i) else Nothing


parseConf :: String -> Maybe [ECSConfig]
parseConf s = do
    let regionLns = filter (\l -> (head $ words l) == "region") (lines s)
    regionStrs <- sequence (map (\l -> words l `at` 2) regionLns)
    regionIds  <- sequence (map readRegion regionStrs)
    return [ECSConfig regionIds]

readRegion :: String -> Maybe RegionId
readRegion "cn-shenzhen" = Just CnShenZhen
readRegion "cn-hongkong" = Just CnHongKong
readRegion _ = Nothing
