{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header, body)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Control.Lens
import Data.Aeson
import Data.Text (unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import GHC.Generics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import System.Logger (Logger)
import Test.Tasty
import Types
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API
import qualified Gundeck.Options as Opts
import qualified System.Logger   as Logger

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { gundeck   :: Endpoint
  , cannon    :: Endpoint
  , brig      :: Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

main :: IO ()
main = withOpenSSL $ runTests go
  where
    go g i = withResource (getOpts g i) releaseOpts $ \opts -> API.tests opts

    getOpts gFile iFile = do
        m <- newManager tlsManagerSettings {
            managerResponseTimeout = responseTimeoutMicro 300000000
        }
        let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
        gConf <- handleParseError =<< decodeFileEither gFile
        iConf <- handleParseError =<< decodeFileEither iFile
        g <- Gundeck . mkRequest <$> optOrEnv gundeck iConf (local . read) "GUNDECK_WEB_PORT"
        c <- Cannon  . mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
        b <- Brig    . mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
        ch <- optOrEnv (\v -> v^.Opts.cassandra.casEndpoint.epHost) gConf pack "GUNDECK_CASSANDRA_HOST"
        cp <- optOrEnv (\v -> v^.Opts.cassandra.casEndpoint.epPort) gConf read "GUNDECK_CASSANDRA_PORT"

        lg <- Logger.new Logger.defSettings
        db <- initCassandra (Endpoint ch cp) lg

        return $ API.TestSetup m g c b db 

    releaseOpts _ = return ()

initCassandra :: Endpoint -> Logger -> IO Cql.ClientState
initCassandra ep lg =
    Cql.init lg $ Cql.setPortNumber (fromIntegral $ ep^.epPort)
                . Cql.setContacts (unpack (ep^.epHost)) []
                . Cql.setKeyspace (Cql.Keyspace "gundeck_test")
                $ Cql.defSettings

mkRequest :: Endpoint -> Request -> Request
mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val
