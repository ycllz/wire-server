{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Proxy.Options
    ( Opts
    , parseConfigOrOptions
    , hostname
    , port
    , config
    , httpPoolSize
    , maxConns
    ) where

import Control.Lens
import Data.Monoid
import Data.Word
import Control.Monad (mzero)
import Data.Yaml (FromJSON(..), (.:), decodeFileEither, ParseException)
import Options.Applicative
import System.Directory
import System.IO (hPutStrLn, stderr)

import qualified Data.Yaml as Y

data Opts = Opts
    { _hostname     :: !String
    , _port         :: !Word16
    , _config       :: !FilePath
    , _httpPoolSize :: !Int
    , _maxConns     :: !Int
    }

makeLenses ''Opts

instance FromJSON Opts where
  parseJSON (Y.Object v) =
    Opts <$>
    v .: "host" <*>
    v .: "port" <*>
    v .: "config" <*>
    v .: "http-pool-size" <*>
    v .: "max-conns"
  parseJSON _ = mzero

parseConfigOrOptions :: IO Opts
parseConfigOrOptions = do
  path <- parseConfigPath
  file <- doesFileExist path
  if file then do
    configFile <- decodeConfigFile path
    case configFile of
      Left err -> fail $ show err
      Right opts -> return opts
  else do
    hPutStrLn stderr $ "Config file at " ++ path ++ " does not exist, fallback to command-line arguments. \n"
    parseOptions

decodeConfigFile :: FilePath -> IO (Either ParseException Opts)
decodeConfigFile = decodeFileEither

parseConfigPath :: IO String
parseConfigPath = execParser (info (helper <*> pathParser) desc)
  where
    pathParser :: Parser String
    pathParser = strOption $
                  long "config-file"
                  <> short 'c'
                  <> help "Config file to load"
                  <> showDefault
                  <> value "/etc/wire/proxy/proxy.yaml"

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    optsParser :: Parser Opts
    optsParser = Opts
        <$> (strOption $
                long "host"
                <> value "*4"
                <> showDefault
                <> metavar "HOSTNAME"
                <> help "host to listen on")

        <*> (option auto $
                long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "listen port")

        <*> (strOption $
                long "config"
                <> metavar "FILE"
                <> help "File containing upstream secrets"
                <> action "file")

        <*> (option auto $
                long "http-pool-size"
                <> metavar "SIZE"
                <> showDefault
                <> help "number of connections for the http pool"
                <> value 256)

        <*> (option auto $
                long "max-connections"
                <> metavar "SIZE"
                <> help "maximum number of incoming connections")

desc :: InfoMod a
desc = header "Proxy - 3rd party proxy" <> fullDesc


