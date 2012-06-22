{-# LANGUAGE OverloadedStrings #-}

module Commistory.Config where

import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec (parse, IResult(..), Result(..))
import Data.Text (Text)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)

import qualified Data.ByteString.Char8 as BS
-- Aeson's "encode" to json generates lazy bytestrings
import qualified Data.ByteString.Lazy.Char8 as BSL

data Config = Config { cfgProjectName :: Text
                     , cfgRepositories :: [CfgRepo]
                     } deriving (Show)

data CfgRepo = CfgRepo { repoName :: Text
                       , repoPath :: Text
                       , repoFileFilterOutPrefix :: [FilePath]
                       } deriving (Show)


instance FromJSON Config where
    parseJSON (Object v) = Config <$> v .: "cfgProjectName"
                                  <*> v .: "cfgRepositories"
    parseJSON _ = mzero

instance ToJSON Config where
    toJSON o = object [ "cfgProjectName" .= cfgProjectName o
                      , "cfgRepositories" .= cfgRepositories o
                      ]


instance FromJSON CfgRepo where
    parseJSON (Object v) = CfgRepo <$> v .: "repoName"
                                   <*> v .: "repoPath"
                                   <*> v .: "repoFileFilterOutPrefix"
    parseJSON _ = mzero

instance ToJSON CfgRepo where
    toJSON o = object [ "repoName" .= repoName o
                      , "repoPath" .= repoPath o
                      , "repoFileFilterOutPrefix" .= repoFileFilterOutPrefix o
                      ]


readFile :: FilePath -> IO (Maybe Config)
readFile path = do bs <- BS.readFile path
                   case parse json bs of
                     (Done _rest r) -> return (T.parseMaybe parseJSON r :: Maybe Config)
                     _              -> return Nothing

toString :: Config -> String
toString cfg = BSL.unpack $ encode cfg

-- parseMsgFromString :: String -> Maybe Config
-- parseMsgFromString s = let bs = BS.pack s
--                        in case parse json bs of
--                           (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Config
--                           _             -> Nothing
-- 
-- exampleJSONMessage :: String
-- exampleJSONMessage = "{\n\"cfgRepositories\":[{\"repoName\":\"myprj\",\"repoPath\":\"some/path\",\"repoFileFilterOutPrefix\":[]}],\"cfgProjectName\":\"hello Aeson!\"}"
-- 
-- testConfig ::IO ()
-- testConfig = do
--     print $ parseMsgFromString exampleJSONMessage
--     let reply = Config { cfgProjectName="hello Aeson!"
--                        , cfgRepositories = [ CfgRepo { repoName = "myprj"
--                                                      , repoPath = "path/to/repo"
--                                                      , repoFileFilterOutPrefix = []
--                                                      }
--                                            ]
--                        }
--     putStrLn $ "Encoded reply: " ++ (BSL.unpack (encode reply))
