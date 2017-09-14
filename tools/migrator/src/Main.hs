{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cassandra as C
import Cassandra.Settings as C
import Control.Lens hiding ((.=))
import Data.Monoid
import Options as O
import Options.Applicative

import qualified System.Logger as Log

main :: IO ()
main = do
    s <- execParser (info (helper <*> settingsParser) desc)
    l <- initLogger
    c <- initCas (s^.rCasSettings) l
    runMigration l c
  where
    desc = header   "migrator"
        <> progDesc "Generic migrator"
        <> fullDesc

    initLogger
        = Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings

    initCas cas l
        = C.init l
        . C.setContacts        (cas^.cHosts) []
        . C.setPortNumber      (fromIntegral $ cas^.cPort)
        . C.setKeyspace        (cas^.cKeyspace)
        . C.setProtocolVersion C.V3
        $ C.defSettings
