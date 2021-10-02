module Main where

import qualified Data.ByteString.Lazy.Char8 as LB8 (putStrLn)

import           Weather
import           Config (config)

main :: IO ()
main = do
  w <- weatherInfo config
  let wd = displayWeather $ mkDisplayWeather w
  LB8.putStrLn wd
