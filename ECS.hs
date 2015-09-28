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
    getQueryString DescribeRegions config =
        getURL config (getRegion config) (URL "https://ecs.aliyuncs.com/") "DescribeRegions" "2014-05-26" []

