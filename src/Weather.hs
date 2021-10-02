{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Weather where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Internal as LB
import           Data.Aeson
import           Network.HTTP.Simple

import           Weather.Data
import qualified LocalConfig as Config
import qualified Api

weatherError :: Text -> WeatherError
weatherError err =
  WeatherError
    { errorDisplay = "🚫"
    , errorDetails = err
    }

weatherInfo :: IO (Either String Config.Config) -> IO (Either String WeatherData)
weatherInfo config = do
  config' <- config
  case config' of

    Right c -> do
      let url = Api.url c Api.apiUrlComponents
      response <- httpLbs (parseRequest_ url)
      return $ eitherDecode (getResponseBody response)

    Left err   -> pure $ Left err

currentTemp :: Either String WeatherData -> Either String Text
currentTemp (Right wd) = Right $ normaliseTemp $ temp $ details wd
currentTemp (Left err) = Left err

feelsLikeTemp :: Either String WeatherData -> Either String Text
feelsLikeTemp (Right wd) = Right $ normaliseTemp $ temp $ details wd
feelsLikeTemp (Left err) = Left err

kToC :: Double -> Double
kToC d = d - 273.15

normaliseTemp :: Double -> Text -- Add F later
normaliseTemp d =  T.pack $ show (round $ kToC d :: Int) <> " C"

currentCondition :: Either String WeatherData -> Either String Text
currentCondition (Right wd) =
  case Map.lookup (icon $ Prelude.head $ weather wd) weatherIcons of
    Just i  -> Right i
    Nothing -> Right ""
currentCondition (Left err) = Left err

currentDescription :: Either String WeatherData -> Either String Text
currentDescription (Right wd) = Right $ T.toUpper (T.take 1 cond) <> T.tail cond
  where cond = description $ Prelude.head $ weather wd
currentDescription (Left err) = Left err

mkDisplayWeather :: Either String WeatherData -> Either String DisplayWeather
mkDisplayWeather w = do
  t <- currentTemp w
  fLt <- feelsLikeTemp w

  cond <- currentCondition w
  desc <- currentDescription w

  return $ DisplayWeather
    { barText = cond <> " " <> t
    , tooltip = desc <> "\n" <> "Feels like: " <> fLt }

displayWeather :: Either String DisplayWeather -> LB.ByteString
displayWeather (Right dw) = encode dw
displayWeather (Left err)   = encode $ weatherError $ T.pack err
