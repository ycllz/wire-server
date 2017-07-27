{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Journal where

import Cassandra as C
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.ByteString.Conversion
import Data.Monoid ((<>))
import Proto.TeamEvents
import Galley.Types.Teams
import System.Logger (Logger)

import qualified System.Logger          as Log
import qualified Galley.Data            as Data
import qualified Galley.Intra.Journal   as Journal
import qualified Galley.Aws             as Aws


runCommand :: Logger -> Aws.Env -> ClientState -> Maybe TeamId -> IO ()
runCommand l env c start = void $ C.runClient c $ do
    page <- case start of
        Just st  -> retry x5 $ paginate teamSelectFrom (paramsP Quorum (Identity st) 100)
        Nothing -> retry x5 $ paginate teamSelect' (paramsP Quorum () 100)
    scan 0 journal page

  where
    scan :: Int
         -> (TeamId -> C.Client ())
         -> Page (TeamId, Bool)
         -> C.Client ()
    scan acc f page = do
        let boundTeams = fst <$> filter snd (result page)
        void $ mapConcurrently f boundTeams
        let count = acc + Prelude.length boundTeams
        Log.info l . Log.msg $ Log.val ("Processed " <> toByteString' count <> " bound teams so far")
        when (hasMore page) $
            retry x5 (liftClient (nextPage page)) >>= scan count f

    journal :: TeamId -> C.Client ()
    journal tid = C.runClient c $ do
        mems <- Data.teamMembers tid
        liftIO $ journalTeamCreate tid mems

    journalTeamCreate :: TeamId -> [TeamMember] -> IO ()
    journalTeamCreate tid mems = do
        now <- Journal.nowInt
        let bUsers = view userId <$> filter (`hasPermission` SetBilling) mems
        let eData = Journal.evData (fromIntegral $ length mems) bUsers
        let event = TeamEvent TeamEvent'TEAM_CREATE (Journal.bytes tid) now (Just eData)
        Aws.execute env (Aws.enqueue event)

-- CQL queries
teamSelect' :: PrepQuery R () (TeamId, Bool)
teamSelect' = "SELECT team, binding FROM team"

teamSelectFrom :: PrepQuery R (Identity TeamId) (TeamId, Bool)
teamSelectFrom = "SELECT team, binding FROM team WHERE token(team) > token(?)"


