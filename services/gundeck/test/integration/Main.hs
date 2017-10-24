{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge (host, port, Request)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Control.Monad (join)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import GHC.Generics
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment
import System.Logger (Logger)
import Test.Tasty
import Types

import qualified API
import qualified Gundeck.Options     as Opts
import qualified System.Logger       as Logger
import qualified Util.Options.Common as Opts

data Config = Config
  -- internal endpoints
  { gundeck :: Opts.Endpoint
  , cannon  :: Opts.Endpoint
  , brig    :: Opts.Endpoint
  } deriving (Show, Generic)

instance FromJSON Config

main :: IO ()
main = withOpenSSL $ do
  (iPath, gPath) <- parseConfigPaths
  iConf <- join $ handleParseError <$> decodeFileEither iPath
  gConf <- join $ handleParseError <$> decodeFileEither gPath

  runTests iConf gConf

runTests :: Maybe Config -> Maybe Opts.Opts -> IO ()
runTests iConf gConf = do
    let local p = Opts.Endpoint { Opts.host = "127.0.0.1", Opts.port = p }
    g <- Gundeck . mkRequest <$> Opts.optOrEnv gundeck iConf (local . read) "GUNDECK_WEB_PORT"
    c <- Cannon  . mkRequest <$> Opts.optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
    b <- Brig    . mkRequest <$> Opts.optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
    casHost <- Opts.optOrEnv (Opts.host . Opts.endpoint . Opts.cassandra) gConf pack "GUNDECK_CASSANDRA_HOST"
    casPort <- Opts.optOrEnv (Opts.port . Opts.endpoint . Opts.cassandra) gConf read "GUNDECK_CASSANDRA_PORT"

    lg <- Logger.new Logger.defSettings
    db <- initCassandra (Opts.Endpoint casHost casPort) lg
    tests <- API.tests g c b db
    defaultMain tests

initCassandra :: Opts.Endpoint -> Logger -> IO Cql.ClientState
initCassandra ep lg =
    Cql.init lg $ Cql.setPortNumber (fromIntegral $ Opts.port ep)
                . Cql.setContacts (unpack (Opts.host ep)) []
                . Cql.setKeyspace (Cql.Keyspace "gundeck_test")
                $ Cql.defSettings

mkRequest :: Opts.Endpoint -> Request -> Request
mkRequest (Opts.Endpoint h p) = host (encodeUtf8 h) . port p

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val

parseConfigPaths :: IO (String, String)
parseConfigPaths = do
  args <- getArgs
  let desc = header "Gundeck Integration tests" <> fullDesc
      res = getParseResult $ execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
  pure $ fromMaybe (defaultIntPath, defaultBrigPath) res
  where
    defaultBrigPath = "/etc/wire/gundeck/conf/gundeck.yaml"
    defaultIntPath = "/etc/wire/gundeck/conf/integration.yaml"
    pathParser :: Parser (String, String)
    pathParser = (,) <$>
                 (strOption $
                 long "integration-config-file"
                 <> short 'i'
                 <> help "Integration config to load"
                 <> showDefault
                 <> value defaultIntPath)
                 <*>
                 (strOption $
                 long "brig-config-file"
                 <> short 'c'
                 <> help "Gunbdeck application config to load"
                 <> showDefault
                 <> value defaultBrigPath)
