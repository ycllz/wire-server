{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData    #-}

module CargoHold.Options where

import CargoHold.CloudFront (Domain (..), KeyPairId (..))
import Control.Applicative
import Data.Monoid
import Data.Text (Text)
import Data.Word
import Data.Yaml (FromJSON(..))
import GHC.Generics
import Options.Applicative
import Util.Options.Common

import qualified Data.Text             as T
import qualified Ropes.Aws             as Aws

data AWSOpts = AWSOpts
    { keyId        :: !(Maybe Aws.AccessKeyId)
    , secretKey    :: !(Maybe Aws.SecretAccessKey)
    , s3Bucket     :: Text
    , cfDomain     :: Domain
    , cfKeyPairId  :: KeyPairId
    , cfPrivateKey :: FilePath
    } deriving (Show, Generic)

instance FromJSON AWSOpts

data Settings = Settings
    { maxTotalBytes :: !Int
    } deriving (Show, Generic)

instance FromJSON Settings

data Opts = Opts
    { cargohold   :: !Endpoint
    , aws         :: !AWSOpts
    , settings    :: !Settings
    } deriving (Show, Generic)

instance FromJSON Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "CargoHold - Asset Service" <> fullDesc

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
    <*> awsParser
    <*> settingsParser
  where
    awsParser :: Parser AWSOpts
    awsParser = AWSOpts <$>
            (optional . fmap Aws.AccessKeyId . bytesOption $
                long "aws-key-id"
                <> metavar "STRING"
                <> help "AWS Access Key ID")
        <*> (optional . fmap Aws.SecretAccessKey . bytesOption $
                long "aws-secret-key"
                <> metavar "STRING"
                <> help "AWS Secret Access Key")

        <*> (fmap T.pack . strOption $
                long "aws-s3-bucket"
                <> metavar "STRING"
                <> help "S3 bucket name")

        <*> (fmap Domain . textOption $
                long "aws-cloudfront-domain"
                <> metavar "STRING"
                <> help "AWS CloudFront Domain")

        <*> (fmap KeyPairId . textOption $
                long "aws-cloudfront-keypair-id"
                <> metavar "STRING"
                <> help "AWS CloudFront Keypair ID")

        <*> strOption
                (long "aws-cloudfront-private-key"
                <> metavar "FILE"
                <> help "AWS CloudFront Private Key")

    settingsParser :: Parser Settings
    settingsParser = Settings <$>
        option auto
            (long "max-total-bytes"
            <> metavar "INT"
            <> value (25 * 1024 * 1024)
            <> showDefault
            <> help "Maximum allowed size in bytes for uploads")
