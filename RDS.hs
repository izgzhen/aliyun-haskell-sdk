module RDS where


import Action
import HTTP
import Auth
import API

-- data Engine = MySQL
--             | SQLServer
--             | PostgreSQL
--             | PPAS
--             deriving (Show, Eq)

-- type EngineVersion = Float

newtype DBInstanceId = DBInstanceId { unDBInstanceId :: String }

newtype DBName = DBName { unDBName :: String }

newtype DBDescription = DBDescription { unDBDescription :: String }

data CharacterSetName = UTF8
                      | GBK
                      | LATIN1
                      | UTF8MB4


instance Param DBInstanceId where
    stringify (DBInstanceId id) = [("DBInstanceId", id)]

instance Param DBName where
    stringify (DBName n) = [("DBName", n)]

instance Param DBDescription where
    stringify (DBDescription des) = [("DBDescription", des)]

instance Param CharacterSetName where
    stringify UTF8    = [("CharacterSetName", "utf8")]
    stringify GBK     = [("CharacterSetName", "gbk")]
    stringify LATIN1  = [("CharacterSetName", "latin1")]
    stringify UTF8MB4 = [("CharacterSetName", "utf8mb4")]

data RDSAction = CreateDatabase DBInstanceId DBName CharacterSetName (Maybe DBDescription)

-- But, is ther more intell
instance Action RDSAction where
    getQueryString (CreateDatabase dbinstid dbname csname maybeDescription) config =
            getURL config
                   (getRegion config)
                   (URL "https://rds.aliyuncs.com/")
                   "CreateDatabase"
                   "2014-08-15"
                   (concat [stringify dbinstid,
                            stringify dbname,
                            stringify csname,
                            stringify maybeDescription])
        where
            f Nothing = []
            f (Just des) = [("DBDescription", des)]



