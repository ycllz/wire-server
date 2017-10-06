module Main (main) where

import Brig.API
import OpenSSL (withOpenSSL)

import Brig.Options
import Control.Exception
import Data.Yaml (decodeFileEither, ParseException)

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

main :: IO ()
main = withOpenSSL $ do
  configFile <- decodeConfigFile "brig.yaml"
  case configFile of
    -- TODO: report config file errors
    Left err -> print err --parseOptions >>= runServer
    Right opts -> putStrLn "ok" --runServer opts
