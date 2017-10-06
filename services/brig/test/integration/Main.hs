{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Bilge (newManager, host, port, Request)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Data.Aeson
import Data.ByteString.Char8 (pack)
import Data.Text (unpack)
import Data.Word
import Data.Yaml (decodeFileEither, ParseException)
import GHC.Generics
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import System.Logger (Logger)
import Test.Tasty

import qualified API           as User
import qualified API.Provider  as Provider
import qualified API.Search    as Search
import qualified API.Team      as Team
import qualified API.TURN      as TURN
import qualified API.User.Auth as UserAuth
import qualified Brig.Options  as Opts
import qualified System.Logger as Logger

-- TODO: move to common lib

data Endpoint = Endpoint String Word16
  deriving (Show, Generic)

data Config = Config
  -- internal endpoints
  { confBrig      :: Endpoint
  , confCannon    :: Endpoint
  , confGalley    :: Endpoint

  , cert          :: FilePath
  } deriving (Show, Generic)

instance FromJSON Endpoint
instance FromJSON Config

decodeConfigFile :: (FromJSON c) => FilePath -> IO (Either ParseException c)
decodeConfigFile = decodeFileEither

runTests :: (Config, Opts.Opts) -> IO ()
runTests (iConf, bConf) = do
    let brig      = mkRequest $ confBrig iConf
        cannon    = mkRequest $ confCannon iConf
        galley    = mkRequest $ confGalley iConf
        turnFile  = Opts.servers . Opts.turn $ bConf
        cassandra = Opts.endpoint . Opts.cassandra $ bConf

    lg <- Logger.new Logger.defSettings
    db <- initCassandra cassandra lg
    mg <- newManager tlsManagerSettings

    userApi     <- User.tests bConf mg brig cannon galley
    userAuthApi <- UserAuth.tests bConf mg lg brig
    providerApi <- Provider.tests bConf (cert iConf) mg db brig cannon galley
    searchApis  <- Search.tests mg brig
    teamApis    <- Team.tests mg brig cannon galley
    turnApi     <- TURN.tests mg brig turnFile

    defaultMain $ testGroup "Brig API Integration"
        [ userApi
        , userAuthApi
        , providerApi
        , searchApis
        , teamApis
        , turnApi
        ]

main :: IO ()
main = withOpenSSL $ do
  iConf <- decodeConfigFile "/etc/wire/integration.yaml"
  bConf <- decodeConfigFile "/etc/wire/brig.yaml"

  let errorOrConfs = (,) <$> iConf <*> bConf
  case errorOrConfs of
    Left errs -> print errs
    Right confs -> runTests confs

initCassandra :: Opts.Endpoint -> Logger -> IO Cql.ClientState
initCassandra ep lg =
    Cql.init lg $ Cql.setPortNumber (fromIntegral $ Opts.port ep)
                . Cql.setContacts (unpack (Opts.host ep)) []
                . Cql.setKeyspace (Cql.Keyspace "brig_test")
                $ Cql.defSettings

mkRequest :: Endpoint -> Request -> Request
mkRequest (Endpoint h p) = host (pack h) . port p
