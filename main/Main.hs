{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (for_)
import           Data.Function (on)
import           Data.List (groupBy,sortOn)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar (Day, fromGregorian, toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import           Formatting (sformat, (%))
import qualified Formatting as F
import qualified Formatting.Time as F
import           GHC.IO.Handle.FD (stderr)
import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Options.Applicative
import           Servant.Client
import           System.Exit (exitFailure, exitFailure)
import           System.IO (hPutStrLn)

import           Network.Hoggl
import           Network.Hoggl.Types
import           Network.Hoggl.Pretty

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> hoggleArgsParser)
                    (fullDesc
                  <> progDesc "Haskell client for Toggl."
                  <> header "hoggl - the Haskell Toggl client.")

run :: HoggleArgs -> IO ()
run (HoggleArgs auth _ TimeToday) = do
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager togglBaseUrl
  e <- runClientM (timeEntriesToday auth) clientEnv
  case e of
    Left _ -> die "There was an error."
    Right ts -> do
      ds <- traverse calcDuration ts
      T.putStrLn (pretty (sum ds))

run (HoggleArgs auth _ TimeWeek) = do
  day <- utctDay <$> getCurrentTime
  let (year,weekNr,dow) = toWeekDate day
  doReport auth (fromWeekDate year weekNr 1) (fromWeekDate year weekNr dow)

run (HoggleArgs auth _ TimeMonth) = do
  day <- utctDay <$> getCurrentTime
  let (year,month,dom) = toGregorian day
  doReport auth (fromGregorian year month 1) (fromGregorian year month dom)

run (HoggleArgs auth _ StartTimer) = do
  e <- tryStartDefault auth
  case e of
    Left _ -> die "Failed to start timer."
    Right _ -> return ()

run (HoggleArgs auth _ StopTimer) = do
  e <- tryStopRunning auth
  case e of
    Left _ -> die "Failed to stop timer."
    Right _ -> return ()

run (HoggleArgs auth _ Info) = do
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager togglBaseUrl
  e <- runClientM (listWorkspaces auth) clientEnv
  case e of
    Left _ -> die "Failed to get workspaces."
    Right ws -> do
      putStrLn "Workspaces:"
      for_ ws (putStrLn . ("- " <>) . workspacePretty)

run (HoggleArgs auth workHours HowLong) = do
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager togglBaseUrl
  start <- utctDay <$> getCurrentTime
  eCurLogged <- runClientM (timeEntriesFromTillNow auth start) clientEnv
  case eCurLogged of
    Left _ -> die "Failed to get time entries."
    Right ts -> do
      worked <- sum <$> traverse calcDuration ts
      let diff = (fromIntegral workHours * 60 * 60) - worked
      T.putStrLn (sformat ("Target of " % F.int % " hours reached: " % F.diff True) workHours diff)

run (HoggleArgs auth _ (Report rSince rUntil)) = do
  tSince <- parseTimeM True defaultTimeLocale dateFormat rSince
  tUntil <- case rUntil of
    Just rUntil' -> parseTimeM True defaultTimeLocale dateFormat rUntil'
    Nothing -> utctDay <$> getCurrentTime
  doReport auth tSince tUntil

doReport :: Token -> Day -> Day -> IO ()
doReport auth tSince tUntil = do
  manager <- newManager tlsManagerSettings
  let clientEnv = mkClientEnv manager togglBaseUrl
  eResult <- runClientM (do
    ws <- listWorkspaces auth
    when (length ws /= 1) (liftIO $ die "Ambiguous workspace")
    detailedReport auth
                   (wsId (head ws))
                   (ISO6801Date tSince)
                   (ISO6801Date tUntil)
                   "hoggl") clientEnv
  case eResult of
    Left e -> do
      print e
      die "Failed to get report."
    Right report -> do
      for_ (groupBy ((==) `on` (utctDay . unpack . teStart)) . sortOn teStart . drData $ report) $ \tesPerDay -> do
        durations <- traverse calcDuration tesPerDay
        T.putStrLn (T.pack (formatTime defaultTimeLocale dateFormat (unpack (teStart (head tesPerDay))))
                 <> ": "
                 <> pretty (sum durations))
      T.putStrLn ("Total: " <> pretty @Double (fromIntegral (drTotalGrand report)))
  where unpack (ISO6801 x) = x

data HoggleArgs = HoggleArgs Token Integer HoggleCmd
data HoggleCmd = TimeToday
               | TimeWeek
               | TimeMonth
               | StartTimer
               | StopTimer
               | HowLong
               | Info
               | Report {reportSince :: String
                        ,reportUntil :: Maybe String
                        }

token :: Parser Token
token = Api <$> strOption (long "token" <> help "API Token")

workHoursOpt :: Parser Integer
workHoursOpt = option auto (long "hours"
                         <> value 8
                         <> showDefault
                         <> help "Number of hours to work per day.")

todayCmd :: Mod CommandFields HoggleCmd
todayCmd = command "today" (info (pure TimeToday) (progDesc "List today's time."))

weekCmd :: Mod CommandFields HoggleCmd
weekCmd = command "week" (info (pure TimeWeek) (progDesc "List this week's time."))

monthCmd :: Mod CommandFields HoggleCmd
monthCmd = command "month" (info (pure TimeMonth) (progDesc "List this month's time."))

startTimerCmd :: Mod CommandFields HoggleCmd
startTimerCmd = command "start" (info (pure StartTimer) (progDesc "Start a timer."))

stopTimerCmd :: Mod CommandFields HoggleCmd
stopTimerCmd = command "stop" (info (pure StopTimer) (progDesc "Stop the current timer."))

howLongCmd :: Mod CommandFields HoggleCmd
howLongCmd = command "howlong" (info (pure HowLong) (progDesc "How long until 8h reached."))

reportCmd :: Mod CommandFields HoggleCmd
reportCmd = command "report" (info (Report <$> strArgument (metavar "SINCE")
                                           <*> optional (strArgument (metavar "UNTIL")))
                                   (progDesc "Request a report for the specified time range."))

infoCmd :: Mod CommandFields HoggleCmd
infoCmd = command "info" (info (pure Info) (progDesc "Display workspaces, clients and projects"))

hoggleArgsParser :: Parser HoggleArgs
hoggleArgsParser = HoggleArgs
               <$> token
               <*> workHoursOpt
               <*> subparser (todayCmd
                           <> weekCmd
                           <> monthCmd
                           <> startTimerCmd
                           <> stopTimerCmd
                           <> howLongCmd
                           <> reportCmd
                           <> infoCmd)

die :: String -> IO ()
die msg = hPutStrLn stderr msg >> exitFailure

dateFormat :: String
dateFormat = "%Y-%m-%d"
