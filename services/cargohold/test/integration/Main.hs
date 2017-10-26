{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header, body)
import Control.Lens
import Data.Monoid
import Data.Proxy
import Data.Tagged
import Data.Typeable (Typeable)
import Data.Yaml hiding (Parser)
import GHC.Generics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL
import Options.Applicative
import Util.Options as Opts
import Util.Options.Common as Opts
import Test.Tasty
import Test.Tasty.Options

import qualified API.V3
import qualified CargoHold.Options as Opts

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/cargohold/conf/cargohold.yaml"
  parseValue = fmap ServiceConfigFile . safeRead
  optionName = return "service-config-file"
  optionHelp = return "Service config file to read from"
  optionCLParser =
    fmap ServiceConfigFile $ strOption $
      (  long (untag (optionName :: Tagged ServiceConfigFile String))
      <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
      )

newtype IntegrationConfigFile = IntegrationConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption IntegrationConfigFile where
  defaultValue = IntegrationConfigFile "/etc/wire/conf/integration.yaml"
  parseValue = fmap IntegrationConfigFile . safeRead
  optionName = return "integration-config-file"
  optionHelp = return "Integration config file to read from"
  optionCLParser =
    fmap IntegrationConfigFile $ strOption $
      (  long (untag (optionName :: Tagged IntegrationConfigFile String))
      <> help (untag (optionHelp :: Tagged IntegrationConfigFile String))
      )

data IntegrationConfig = IntegrationConfig
  -- internal endpoint
  { cargohold :: Opts.Endpoint
  } deriving (Show, Generic)

instance FromJSON IntegrationConfig

main :: IO ()
main = withOpenSSL runTests'

runTests' :: IO ()
runTests' = do
    defaultMainWithIngredients ings $ 
        askOption $ \(ServiceConfigFile c) ->
        askOption $ \(IntegrationConfigFile i) -> 
            withResource (getOpts c i) releaseOpts $ \opts ->
              testGroup "Cargohold API Integration" [API.V3.tests opts]
  where
    getOpts _ i = do
        -- TODO: It would actually be useful to read some
        -- values from cargohold (max bytes, for instance)
       -- so that tests do not need to keep those values
        -- in sync and the user _knows_ what they are
        m <- newManager tlsManagerSettings {
            managerResponseTimeout = responseTimeoutMicro 300000000
        }
        let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
        iConf <- handleParseError =<< decodeFileEither i
        cp <- Opts.optOrEnv cargohold iConf (local . read) "CARGOHOLD_WEB_PORT"
        let cg = host "127.0.0.1" . port (cp^.epPort)
        return $ API.V3.TestSetup m cg
    releaseOpts _ = return ()

    ings =
      includingOptions 
        [Option (Proxy :: Proxy ServiceConfigFile)
        ,Option (Proxy :: Proxy IntegrationConfigFile)
        ]
      : defaultIngredients

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val
