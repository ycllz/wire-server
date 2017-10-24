{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Options.Common where

import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import Data.Word (Word16)
import Data.Yaml hiding (Parser)
import GHC.Generics
import Options.Applicative
import System.Environment

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

data Endpoint = Endpoint
    { host :: !Text
    , port :: !Word16
    } deriving (Show, Generic)

instance FromJSON Endpoint

data CassandraOpts = CassandraOpts
    { endpoint :: !Endpoint
    , keyspace :: !Text
    } deriving (Show, Generic)

instance FromJSON CassandraOpts

optOrEnv :: (a -> b) -> (Maybe a) -> (String -> b) -> String -> IO b
optOrEnv getter conf reader var = case conf of
    Nothing -> reader <$> getEnv var
    Just c  -> pure $ getter c

bytesOption :: Mod OptionFields String -> Parser ByteString
bytesOption = fmap C.pack . strOption

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption

cassandraParser :: Parser CassandraOpts
cassandraParser = CassandraOpts <$>
    (Endpoint <$>
        (textOption $
            long "cassandra-host"
            <> metavar "HOSTNAME" 
            <> help "Cassandra hostname or address")
      <*>
        (option auto $
            long "cassandra-port"
            <> metavar "PORT"
            <> help "Cassandra port"))
  <*>
    (textOption $
        long "cassandra-keyspace"
        <> metavar "STRING"
        <> help "Cassandra keyspace")

discoUrlParser :: Parser Text
discoUrlParser = textOption
    $ long "disco-url" 
    <> metavar "URL"
    <> help "klabautermann url"
