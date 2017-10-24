{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Types.AddressBook
    ( module Brig.Types.AddressBook
    ) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Id
import Data.Json.Util

import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding     as T

-- The base64-encoded SHA-256 of an email address or a phone number
newtype Entry = Entry { abEntrySha256 :: ByteString }
    deriving (Eq, Show, Ord)

instance FromJSON Entry where
    parseJSON = withText "Entry" $
        either (fail "Invalid Entry") (pure . Entry) . (B64.decode . T.encodeUtf8)

-- Used only in tests but defined here to avoid orphan
instance ToJSON Entry where
    toJSON = String . T.decodeUtf8 . B64.encode . abEntrySha256

data Card = Card
    { cEntries :: ![Entry]
    } deriving (Eq, Show)

instance FromJSON Card where
    parseJSON = withObject "matching-card" $ \o ->
        Card  <$> o .:  "contact"

instance ToJSON Card where
    toJSON c = object
        [ "contact" .= cEntries c
        ]

newtype AddressBook = AddressBook
    { abCards :: [Card]
    } deriving (Eq, Show)

instance FromJSON AddressBook where
    parseJSON = withObject "address-book" $ \o ->
        AddressBook <$> o .: "cards"

instance ToJSON AddressBook where
    toJSON ab = object
        [ "cards" .= abCards ab
        ]

-- V3 result

data Match = Match
    { mUser   :: !UserId
    } deriving (Eq, Ord, Show)

instance FromJSON Match where
    parseJSON = withObject "match" $ \o ->
        Match <$> o .:  "id"

instance ToJSON Match where
    toJSON m = object
        $ "id"      .= mUser m
        # []

data MatchingResult = MatchingResult
    { mrMatches :: ![Match]
    , mrAuto    :: ![UserId]
    } deriving (Eq, Ord, Show)

instance FromJSON MatchingResult where
    parseJSON = withObject "matches" $ \o ->
        MatchingResult <$> o .: "results"
                       <*> o .: "auto-connects"

instance ToJSON MatchingResult where
    toJSON r = object
        [ "results"       .= mrMatches r
        , "auto-connects" .= mrAuto r
        ]
