{-# LANGUAGE OverloadedStrings #-}
module Network.Hoggl (currentTimeEntry
                     ,stopTimer
                     ,startTimer
                     ,getTimer
                     ,getEntries
                     ,listWorkspaces
                     ,listProjects
                     ,detailedReport

                     ,tryStartDefault
                     ,tryStopRunning
                     ,prettyCurrent
                     ,timeEntriesDay
                     ,timeEntriesToday
                     ,timeEntriesFromTillNow
                     ,togglBaseUrl
                     ,pretty
                     ,calcDuration
                     ) where

import           Control.Monad.Error.Class
import           Control.Monad.IO.Class (liftIO)
import           Data.Bifunctor (first)
import           Data.Fixed (mod')
import           Data.Proxy (Proxy(Proxy))
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime(..), NominalDiffTime, getCurrentTime, diffUTCTime)
import           Formatting (sformat, (%), float)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client

import           Network.Hoggl.Types

togglBaseUrl :: BaseUrl
togglBaseUrl = BaseUrl Https "toggl.com" 443 "/"

togglApi :: Proxy TogglApi
togglApi = Proxy

currentTimeEntry' :: Maybe Token -> ClientM TimeEntry
stopTimer' :: Maybe Token -> TimeEntryId -> ClientM TimeEntry
startTimer' :: Maybe Token -> TimeEntryStart -> ClientM TimeEntry
getTimer' :: Maybe Token -> TimeEntryId -> ClientM TimeEntry
getEntries' :: Maybe Token -> Maybe ISO6801 -> Maybe ISO6801 -> ClientM [TimeEntry]
listWorkspaces' :: Maybe Token -> ClientM [Workspace]
listProjects' :: Maybe Token -> WorkspaceId -> ClientM [Project]
(currentTimeEntry' :<|> stopTimer' :<|> startTimer' :<|> getTimer' :<|> getEntries' :<|> listWorkspaces' :<|> listProjects') =
  client togglApi

currentTimeEntry :: Token -> ClientM (Maybe TimeEntry)
currentTimeEntry token = (Just <$> currentTimeEntry' (Just token)) `catchError` handler
  where
    handler :: ClientError -> ClientM (Maybe TimeEntry)
    handler (DecodeFailure "{\"data\":null}" _) = return Nothing
    handler e = throwError e

stopTimer :: Token -> TimeEntryId -> ClientM TimeEntry
stopTimer tk = stopTimer' (Just tk)

startTimer :: Token -> TimeEntryStart -> ClientM TimeEntry
startTimer tk = startTimer' (Just tk)

getTimer :: Token -> TimeEntryId -> ClientM TimeEntry
getTimer tk = getTimer' (Just tk)

getEntries :: Token -> ISO6801 -> ISO6801 -> ClientM [TimeEntry]
getEntries tk start end = getEntries' (Just tk) (Just start) (Just end)

listWorkspaces :: Token -> ClientM [Workspace]
listWorkspaces token = listWorkspaces' (Just token)

listProjects :: Token -> WorkspaceId -> ClientM [Project]
listProjects token = listProjects' (Just token)

togglReportApi :: Proxy ToggleReportApi
togglReportApi = Proxy

detailedReport' :: Maybe Token
                -> Maybe WorkspaceId
                -> Maybe ISO6801Date
                -> Maybe ISO6801Date
                -> Maybe Text
                -> ClientM DetailedReport
detailedReport' = client togglReportApi

detailedReport :: Token
               -> WorkspaceId
               -> ISO6801Date
               -> ISO6801Date
               -> Text
               -> ClientM DetailedReport
detailedReport tk wid since untl userAgent = detailedReport' (Just tk)
                                                             (Just wid)
                                                             (Just since)
                                                             (Just untl)
                                                             (Just userAgent)

defaultTimeEntry :: TimeEntryStart
defaultTimeEntry = TES {tesDescription = Nothing
                  ,tesTags = []
                  ,tesPid = Nothing
                  ,tesCreatedWith = "hoggl"
                  }

calcDuration :: TimeEntry -> IO NominalDiffTime
calcDuration te =
  case teStop te of
    Just _ -> return (teDuration te)
    Nothing -> do
      stop <- getCurrentTime
      return (diffUTCTime stop start)
  where ISO6801 start = teStart te

pretty :: RealFrac n => n -> Text
pretty n =
  sformat (float % "h " % float % "m")
          (floorInt $ n / 60 / 60)
          (floorInt $ (n / 60) `mod'` 60)

  where floorInt :: RealFrac n => n -> Integer
        floorInt = floor

prettyCurrent :: Token -> IO ()
prettyCurrent authorization = do
  nmanager <- newManager tlsManagerSettings
  etimer <- runClientM (currentTimeEntry authorization) $ mkClientEnv nmanager togglBaseUrl
  case etimer of
    Right (Just timer) -> calcDuration timer >>= T.putStrLn . pretty
    _ -> return ()

tryStartDefault :: Token -> IO (Either HogglError TimeEntry)
tryStartDefault authorization = do
  nmanager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv nmanager togglBaseUrl
  currentTimer <- runClientM (currentTimeEntry authorization) clientEnv
  case currentTimer of
    Right Nothing ->
      first ServantError <$> runClientM (startTimer authorization defaultTimeEntry) clientEnv
    Right (Just _) -> return (Left (HogglError "There already is a running timer!"))
    Left e -> return (Left (ServantError e))

tryStopRunning :: Token -> IO (Either HogglError TimeEntry)
tryStopRunning authorization = do
  nmanager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv nmanager togglBaseUrl
  currentTimer <- runClientM (currentTimeEntry authorization) clientEnv
  case currentTimer of
    Right (Just TimeEntry {teId = tid}) ->
      first ServantError <$> runClientM (stopTimer authorization tid) clientEnv
    Right Nothing -> return (Left (HogglError "No timer running!"))
    Left e -> return (Left (ServantError e))

timeEntriesDay :: Token -> Day -> ClientM [TimeEntry]
timeEntriesDay authorization day = do
  let start = UTCTime { utctDay = day, utctDayTime = 0 }
      end = start { utctDay = day, utctDayTime =  86399 }
  getEntries authorization (ISO6801 start) (ISO6801 end)

timeEntriesToday :: Token -> ClientM [TimeEntry]
timeEntriesToday authorization = do
  now <- liftIO getCurrentTime
  timeEntriesDay authorization (utctDay now)

timeEntriesFromTillNow :: Token -> Day -> ClientM [TimeEntry]
timeEntriesFromTillNow authorization start = do
  now <- liftIO getCurrentTime
  getEntries authorization (ISO6801 (UTCTime start 0)) (ISO6801 now)
