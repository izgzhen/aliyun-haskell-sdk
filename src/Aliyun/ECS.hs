-- Functionality Modules for ECS

module Aliyun.ECS where

import Aliyun.Config
import Aliyun.API
import Aliyun.Action
import Aliyun.HTTP

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

