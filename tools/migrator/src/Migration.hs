{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Migration where

import Cassandra as C
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.ByteString.Conversion
import Data.Monoid ((<>))
import Data.Int
import Data.Maybe (fromMaybe)
import Proto.TeamEvents
import System.Logger (Logger)

import qualified System.Logger          as Log

runMigration :: Logger -> ClientState -> IO ()
runMigration l c = void $ C.runClient c $ do
    page <- retry x5 $ paginate userSelect' (paramsP Quorum () 100)
    scan 0 page
  where
    scan :: Int -> Page Row -> C.Client ()
    scan acc page = undefined
