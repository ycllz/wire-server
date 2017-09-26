{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Proxy.Options
    ( Opts
    , parseOptions
    , parseConfig
    , hostname
    , port
    , config
    , httpPoolSize
    , maxConns
    ) where

import Control.Lens
import Data.Monoid
import Data.Text
import Options.Applicative

import Dhall hiding (Text, auto)

import qualified Data.Text              as T
import qualified Data.Text.Lazy         as Text.Lazy

data Opts = Opts
    { _hostname     :: !Text
    , _port         :: !Integer
    , _config       :: !Text
    , _httpPoolSize :: !Integer
    , _maxConns     :: !Integer
    } deriving (Generic, Show)

makeLenses ''Opts

instance Interpret Opts

parseConfig :: IO Opts
parseConfig = detailed $ input autoInterpret "./proxy.dhall"
  where
    autoInterpret ::  Interpret a => Type a
    autoInterpret = autoWith
        ( defaultInterpretOptions { fieldModifier = Text.Lazy.dropWhile (== '_') })

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Proxy - 3rd party proxy" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> (textOption $
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

        <*> (textOption $
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

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption

