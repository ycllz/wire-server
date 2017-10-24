{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Gundeck.Options where

-- module Gundeck.Options
--     ( Opts
--     , parseOptions
--     , hostname
--     , serverPort
--     , discoUrl

--       -- * Storage
--     , cassHost
--     , cassPort
--     , keyspace
--     , redisHost
--     , redisPort

--       -- * AWS
--     , queueName
--     , awsRegion
--     , awsAccount
--     , awsArnEnv

--       -- * RPC
--     , httpPoolSize

--       -- * Notifications
--     , NotificationTTL (..)
--     , notificationTTL

--       -- * Fallback Notification Queue
--     , fbSkipFallbacks
--     , fbQueueLimit
--     , fbQueueDelay
--     , fbQueueBurst

import Cassandra hiding (Error)
import Control.Lens hiding ((.=))
import Data.Aeson.Types (typeMismatch)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Data.Misc
import Data.Monoid
import Data.Scientific (toBoundedInteger)
import Data.String
import Data.Word
import Data.Yaml (FromJSON(..), (.:), (.:?))
import GHC.Generics
import Gundeck.Aws.Arn
import Options.Applicative
import Options.Applicative.Types
import Util.Options.Common

import qualified Data.Yaml as Y

defaultNotificationTLL :: Word32
defaultNotificationTLL = 86400
defaultHttpPoolSize :: Int
defaultHttpPoolSize = 128

newtype NotificationTTL = NotificationTTL
    { notificationTTLSeconds :: Word32 }
    deriving (Eq, Ord, Show)

instance FromJSON NotificationTTL where
  parseJSON (Y.Number n) =
    let bounded = toBoundedInteger n :: Maybe Word32
    in pure $ NotificationTTL $ fromMaybe defaultNotificationTLL bounded
  parseJSON v = typeMismatch "notificationTTL" v

instance FromJSON ArnEnv where
  parseJSON (Y.String s) = pure $ ArnEnv s
  parseJSON v            = typeMismatch "arnEnv" v

instance FromJSON Account where
  parseJSON (Y.String s) = pure $ Account s
  parseJSON v            = typeMismatch "account" v

data AWSOpts = AWSOpts
    { account   :: !Account
    , region    :: !Region
    , arnEnv    :: !ArnEnv
    , queueName :: !Text
    } deriving (Show, Generic)

instance FromJSON AWSOpts

data FallbackOpts = FallbackOpts
    { skipFallbacks :: !Bool
    , queueDelay    :: !Word64
    , queueLimit    :: !Int
    , queueBurst    :: !Word16
    } deriving (Show, Generic)

instance FromJSON FallbackOpts

data Settings = Settings
    { httpPoolSize    :: !Int
    , notificationTTL :: !NotificationTTL
    } deriving (Show, Generic)

instance FromJSON Settings

data Opts = Opts
    { gundeck     :: !Endpoint
    , cassandra   :: !CassandraOpts
    , redis       :: !Endpoint
    , aws         :: !AWSOpts
    , discoUrl    :: !(Maybe Text)
    , fallback    :: !FallbackOpts
    , optSettings :: !Settings
    } deriving (Show, Generic)

instance FromJSON Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Gundeck - Push Notifications" <> fullDesc

optsParser :: Parser Opts
optsParser = Opts <$>
    (Endpoint <$>
        (textOption $
            long "host" 
            <> value "*4"
            <> showDefault
            <> metavar "HOSTNAME"
            <> help "Hostname or address to bind to")
        <*>
        (option auto $
            long "port"
            <> short 'p'
            <> metavar "PORT"
            <> help "Port to listen on"))
  <*> cassandraParser
  <*> redisParser
  <*> awsParser
  <*> optional discoUrlParser
  <*> fallbackParser
  <*> settingsParser
  where
    redisParser :: Parser Endpoint
    redisParser = Endpoint <$>
        (textOption $
            long "redis-host" 
            <> metavar "HOSTNAME"
            <> help "Redis hostname")
        <*>
        (option auto $
            long "redis-port"
            <> metavar "PORT"
            <> help "Redis port")

    awsParser :: Parser AWSOpts
    awsParser = AWSOpts <$>
        (fmap Account . textOption $
            long "aws-account"
            <> metavar "STRING"
            <> help "aws account")

        <*> 
        (option parseRegion $
            long "aws-region"
            <> metavar "STRING"
            <> help "aws region name")

        <*> 
        (fmap ArnEnv . textOption $
            long "aws-arn-env"
            <> metavar "STRING"
            <> help "environment name to scope ARNs to")
        <*>
        (textOption $
            long "event-queue-name"
            <> metavar "STRING"
            <> help "sqs queue name")

    fallbackParser :: Parser FallbackOpts
    fallbackParser = FallbackOpts <$>
        -- NOTE: If set, notifications are still queued to be sent, etc. but never actually
        -- end up getting sent out. This allows us to still keep track of how successful
        -- we are with cancelling the fallback notifications and thus get a feeling of
        --  where we stand today.
        (switch $
            long "skip-fallbacks"
            <> help "Use this option if you wish to never send delayed fallback notifications.")

        <*> (delayOption $
                long "fallback-queue-delay"
                <> metavar "SIZE"
                <> showDefault
                <> help "Delay (seconds) of notifications before sending a fallback. \
                   \MUST be higher than 30 seconds."
                <> value 300)

        <*> (option auto $
                long "fallback-queue-limit"
                <> metavar "SIZE"
                <> showDefault
                <> help "Max. size of the notification fallback queue."
                <> value 30000)

        <*> (option auto $
                long "fallback-queue-burst"
                <> metavar "SIZE"
                <> showDefault
                <> help "Max. number of delayed notifications to fire in a row (i.e. per second)."
                <> value 100)

    delayOption = fmap check . option auto
      where
        check x = if x < 30 then error "Delay must > 30" else x

    settingsParser :: Parser Settings
    settingsParser = Settings <$>
        (option auto $
            long "http-pool-size"
            <> metavar "SIZE"
            <> showDefault
            <> help "number of connections for the http client pool"
            <> value defaultHttpPoolSize)
        <*>
        (fmap NotificationTTL . option auto $
            long "notification-ttl"
            <> metavar "SIZE"
            <> showDefault
            <> help "TTL (seconds) of stored notifications"
            <> value defaultNotificationTLL)

    parseRegion = readerAsk >>= either readerError return . fromText . fromString
