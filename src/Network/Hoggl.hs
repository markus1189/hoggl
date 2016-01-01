{-# LANGUAGE OverloadedStrings #-}
module Network.Hoggl (currentTimeEntry
                     ,stopTimer
                     ,startTimer
                     ,getTimer
                     ,getEntries

                     ,tryStartDefault
                     ,tryStopRunning
                     ,prettyCurrent
                     ,timeEntriesDay
                     ,timeEntriesToday

                     ,pretty
                     ,calcDuration
                     ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either (EitherT(..), runEitherT)
import           Data.Bifunctor (first)
import           Data.Fixed (mod')
import           Data.Proxy (Proxy(Proxy))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime(..), NominalDiffTime, getCurrentTime, diffUTCTime, UTCTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)
import           Formatting (sformat, (%), float)
import           Servant.API
import           Servant.Client

import           Network.Hoggl.Types (Token(..)
                                     ,TogglApi
                                     ,TimeEntry(..)
                                     ,TimeEntryId(..)
                                     ,TimeEntryStart(..)
                                     ,ISO6801(..)
                                     ,HogglError(..))

togglAPI :: Proxy TogglApi
togglAPI = Proxy

currentTimeEntry' :: Maybe Token -> EitherT ServantError IO TimeEntry
stopTimer' :: Maybe Token -> TimeEntryId -> EitherT ServantError IO TimeEntry
startTimer' :: Maybe Token -> TimeEntryStart -> EitherT ServantError IO TimeEntry
getTimer' :: Maybe Token -> TimeEntryId -> EitherT ServantError IO TimeEntry
getEntries' :: Maybe Token -> Maybe ISO6801 -> Maybe ISO6801 -> EitherT ServantError IO [TimeEntry]
(currentTimeEntry' :<|> stopTimer' :<|> startTimer' :<|> getTimer' :<|> getEntries') =
  client togglAPI (BaseUrl Https "toggl.com" 443)

currentTimeEntry :: Token -> EitherT ServantError IO (Maybe TimeEntry)
currentTimeEntry token = EitherT $ do
  res <- runEitherT (currentTimeEntry' (Just token))
  case res of
    Left DecodeFailure {responseBody = "{\"data\":null}"} -> return (Right Nothing)
    Left e -> return (Left e)
    Right te -> return (Right (Just te))

stopTimer :: Token -> TimeEntryId -> EitherT ServantError IO TimeEntry
stopTimer tk = stopTimer' (Just tk)

startTimer :: Token -> TimeEntryStart -> EitherT ServantError IO TimeEntry
startTimer tk = startTimer' (Just tk)

getTimer :: Token -> TimeEntryId -> EitherT ServantError IO TimeEntry
getTimer tk = getTimer' (Just tk)

getEntries :: Token -> ISO6801 -> ISO6801 -> EitherT ServantError IO [TimeEntry]
getEntries tk start end = getEntries' (Just tk) (Just start) (Just end)

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
      start <- parseStart te
      return (diffUTCTime stop start)

parseStart :: TimeEntry -> IO UTCTime
parseStart TimeEntry { teStart = s } =
  parseTimeM True
             defaultTimeLocale
             (iso8601DateFormat (Just "%H:%M:%S+00:00"))
             (T.unpack s)

pretty :: RealFrac n => n -> Text
pretty n =
  sformat (float % "h " % float % "m")
          (floorInt $ n / 60 / 60)
          (floorInt $ (n / 60) `mod'` 60)

  where floorInt :: RealFrac n => n -> Integer
        floorInt = floor

prettyCurrent :: Token -> IO ()
prettyCurrent authorization = do
  etimer <- runEitherT (currentTimeEntry authorization)
  case etimer of
    Right (Just timer) -> calcDuration timer >>= T.putStrLn . pretty
    _ -> return ()

tryStartDefault :: Token -> IO (Either HogglError TimeEntry)
tryStartDefault authorization = do
  currentTimer <- runEitherT (currentTimeEntry authorization)
  case currentTimer of
    Right Nothing ->
      first ServantError <$> runEitherT (startTimer authorization defaultTimeEntry)
    Right (Just _) -> return (Left (HogglError "There already is a running timer!"))
    Left e -> return (Left (ServantError e))

tryStopRunning :: Token -> IO (Either HogglError TimeEntry)
tryStopRunning authorization = do
  currentTimer <- runEitherT (currentTimeEntry authorization)
  case currentTimer of
    Right (Just TimeEntry {teId = tid}) ->
      first ServantError <$> runEitherT (stopTimer authorization tid)
    Right Nothing -> return (Left (HogglError "No timer running!"))
    Left e -> return (Left (ServantError e))

timeEntriesDay :: Token -> Day -> EitherT ServantError IO [TimeEntry]
timeEntriesDay authorization day = do
  let start = UTCTime { utctDay = day, utctDayTime = 0 }
      end = start { utctDay = day, utctDayTime =  86399 }
  getEntries authorization (ISO6801 start) (ISO6801 end)

timeEntriesToday :: Token -> EitherT ServantError IO [TimeEntry]
timeEntriesToday authorization = do
  now <- liftIO getCurrentTime
  timeEntriesDay authorization (utctDay now)
