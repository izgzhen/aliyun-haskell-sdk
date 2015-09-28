-- Functionality Modules for ECS

module ECS where

import Config
import API
import Action
import HTTP

{-
    Three styles of requests:

    * RPC
    * ROA
    * OSS
-}

data ECSRequest = DescribeRegions


instance Action ECSRequest where
    -- getURL :: ECSRequest -> Config -> URL
    getQueryString DescribeRegions config = getURL config (getECSRegion config) (URL "https://ecs.aliyuncs.com/") "DescribeRegions"


getECSRegion config =  head . _regionId . head $  _ecses config
