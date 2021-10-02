{-# LANGUAGE OverloadedStrings #-}

module Api where

import           Config
import           Data.Text (Text)
import qualified Data.Text as T

type ApiUrl = String
-- It would be nice to construct the API request
-- directly from the ApiUrlComponents data, rather
-- than going via a String. But http-conduit makes
-- accessing Request weirdly difficult (or I'm
-- missing something. I'm probably missing something.)

data ApiUrlComponents =
  ApiUrlComponents
    { urlProtocol  :: Config -> Text
    , urlServer    :: Config -> Text
    , urlLocKey    :: Config -> Text
    , urlLocation  :: Config -> Text
    , urlApiKeyKey :: Config -> Text
    , urlApiKey    :: Config -> Text
    }

urlProtocol_ :: Config -> Text
urlProtocol_ conf
  | https . api $ conf = "https://"
  | otherwise          = "http://"

urlServer_ :: Config -> Text
urlServer_ = server_address . api

urlLocKey_ :: Config -> Text
urlLocKey_ = location_key . api

urlLocation_ :: Config -> Text
urlLocation_ = location . api

urlApiKeyKey_ :: Config -> Text
urlApiKeyKey_ = api_key_key . api

urlApiKey_ :: Config -> Text
urlApiKey_ = api_key . api

apiUrlComponents :: ApiUrlComponents
apiUrlComponents =
  ApiUrlComponents
    { urlProtocol  = urlProtocol_
    , urlServer    = urlServer_
    , urlLocKey    = urlLocKey_
    , urlLocation  = urlLocation_
    , urlApiKeyKey = urlApiKeyKey_
    , urlApiKey    = urlApiKey_
    }

url :: Config -> ApiUrlComponents -> ApiUrl
url c u = T.unpack $
 urlProtocol  u c <>
 urlServer    u c <> "/?" <>
 urlLocKey    u c <> "="  <>
 urlLocation  u c <> "&"  <>
 urlApiKeyKey u c <> "="  <>
 urlApiKey    u c
