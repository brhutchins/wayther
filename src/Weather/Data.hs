{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Weather.Data where

import           Data.Text (Text)
import           Data.Aeson
import           GHC.Generics
import           Data.Map (Map)
import qualified Data.Map as Map

data Weather
  = Weather
      { summary     :: !Text
      , description :: !Text
      , icon        :: !Text
      } deriving (Show, Generic)

instance FromJSON Weather where
  parseJSON = withObject "Weather" $ \v ->
    Weather <$> v .: "main"
            <*> v .: "description"
            <*> v .: "icon"

instance ToJSON Weather where
  toJSON (Weather summary description icon) =
    object [ "main"        .= summary
           , "description" .= description
           , "icon"        .= icon
           ]

data WeatherDetails
  = WeatherDetails
      { temp       :: !Double
      , feels_like :: !Double
      } deriving (Show, Generic, FromJSON, ToJSON)

data WeatherData
  = WeatherData
      { weather :: [Weather]
      , details :: WeatherDetails
      } deriving Show

instance FromJSON WeatherData where
  parseJSON = withObject "WeatherData" $ \v ->
    WeatherData <$> v .: "weather"
                <*> v .: "main"

instance ToJSON WeatherData where
  toJSON (WeatherData weather details) =
    object [ "weather" .= weather
           , "main"    .= details
           ]

data DisplayWeather
  = DisplayWeather
      { barText :: Text
      , tooltip :: Text
      } deriving Show

instance ToJSON DisplayWeather where
  toJSON (DisplayWeather barText tooltip) =
    object [ "text" .= barText
           , "tooltip" .= tooltip
           ]

data WeatherError
  = WeatherError
      { errorDisplay :: Text
      , errorDetails :: Text
      } deriving Show

instance ToJSON WeatherError where
  toJSON (WeatherError errorDisplay errorDetails) =
    object [ "text"    .= errorDisplay
           , "tooltip" .= errorDetails
           ]

weatherIcons :: Map Text Text
weatherIcons = Map.fromList
  [ ("01d", "🌞")
  , ("01n", "🌑")
  , ("02d", "🌥")
  , ("02n", "☁")
  , ("03d", "☁")
  , ("03n", "☁")
  , ("04d", "☁")
  , ("04n", "☁")
  , ("09d", "🌧")
  , ("09n", "🌧")
  , ("10d", "🌦")
  , ("10n", "🌧")
  , ("11d", "⛈")
  , ("11n", "⛈")
  , ("13d", "❄")
  , ("13n", "❄")
  , ("50d", "🌫")
  , ("50n", "🌫")
  ]
