{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, RecordWildCards #-}

module Config where

import           Data.Text (Text)
import           Data.Aeson
import           System.Environment.MrEnv (envAsString)
import qualified Data.ByteString.Lazy as B
import           GHC.Generics (Generic)

newtype Config
  = Config
      { api :: ApiConfig
      } deriving (Show, Generic, FromJSON)

data ApiConfig
  = ApiConfig
      { api_key        :: Text
      , location       :: Text
      , server_address :: Text
      , https          :: Bool
      , location_key   :: Text
      , api_key_key    :: Text
      } deriving (Show, Generic)

instance FromJSON ApiConfig where
  parseJSON = withObject "apiconfig" $ \o -> do
    api_key        <- o .:  "api_key"
    location       <- o .:? "location"       .!= "0,0"
    server_address <- o .:? "server_address" .!= "api.openweathermap.org/data/2.5/weather"
    https          <- o .:? "https"          .!= False
    location_key   <- o .:? "location_key"   .!= "q"
    api_key_key    <- o .:? "api_key_key"    .!= "appid"
    pure ApiConfig {..}

configFilePath :: IO FilePath
configFilePath = do
  home <- envAsString "HOME" "~/"
  pure $ home ++
           "/.config/weather-bar-module/config.json"

configFile :: IO B.ByteString
configFile = do
  path <- configFilePath
  B.readFile path

config :: IO (Either String Config)
config = do eitherDecode <$> configFile
