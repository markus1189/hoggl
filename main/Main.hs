{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (when)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Either (runEitherT)
import           Data.Foldable (for_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Calendar (Day)
import           Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import           Data.Time.Clock (UTCTime(..), getCurrentTime, addUTCTime)
import           Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import           GHC.IO.Handle.FD (stderr)
import           Options.Applicative
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
run (HoggleArgs auth _ _ TimeToday) = do
  e <- runEitherT (timeEntriesToday auth)
  case e of
    Left _ -> die "There was an error."
    Right ts -> do
      ds <- traverse calcDuration ts
      T.putStrLn (pretty (sum ds))

run (HoggleArgs auth _ _ StartTimer) = do
  e <- tryStartDefault auth
  case e of
    Left _ -> die "Failed to start timer."
    Right _ -> return ()

run (HoggleArgs auth _ _ StopTimer) = do
  e <- tryStopRunning auth
  case e of
    Left _ -> die "Failed to stop timer."
    Right _ -> return ()

run (HoggleArgs auth _ _ Info) = do
  e <- runEitherT (listWorkspaces auth)
  case e of
    Left _ -> die "Failed to get workspaces."
    Right ws -> do
      putStrLn "Workspaces:"
      for_ ws (putStrLn . ("- " <>) . workspacePretty)

run (HoggleArgs auth lastDow workHours HowLong) = do
  start <- startOfCurrentWeek
  eCurLogged <- runEitherT (timeEntriesFromTillNow auth start)
  case eCurLogged of
    Left _ -> die "Failed to get time entries."
    Right ts -> do
      worked <- sum <$> traverse calcDuration ts
      req <- requiredTime lastDow workHours
      let diff = fromIntegral req - worked
      endTime <- addUTCTime diff <$> getCurrentTime
      let fendTime = formatTime defaultTimeLocale "%R" endTime
      T.putStrLn $ pretty diff <> T.pack (", average reached at " <> fendTime)

run (HoggleArgs auth _ _ (Report rSince rUntil)) = do
  tSince <- parseTimeM True defaultTimeLocale "%d-%m-%y" rSince
  tUntil <- parseTimeM True defaultTimeLocale "%d-%m-%y" rUntil
  eResult <- runEitherT $ do
    ws <- listWorkspaces auth
    when (length ws /= 1) (lift $ die "Ambiguous workspace")
    detailedReport auth
                   (wsId (head ws))
                   (ISO6801Date tSince)
                   (ISO6801Date tUntil)
                   "hoggl"
  case eResult of
    Left _ -> die "Failed to get report."
    Right report -> print report

data HoggleArgs = HoggleArgs Token Integer Integer HoggleCmd
data HoggleCmd = TimeToday
               | StartTimer
               | StopTimer
               | HowLong
               | Info
               | Report {reportSince :: String
                        ,reportUntil :: String
                        }

token :: Parser Token
token = Api <$> strOption (long "token" <> help "API Token")

lastDowOpt :: Parser Integer
lastDowOpt = option auto (long "last-dow"
                       <> short 'l'
                       <> value 5
                       <> showDefault
                       <> help "Last work day of week (1 = Monday)")

workHoursOpt :: Parser Integer
workHoursOpt = option auto (long "work-yours"
                         <> short 'h'
                         <> value 8
                         <> showDefault
                         <> help "Number of hours to work per day in avg.")

todayCmd :: Mod CommandFields HoggleCmd
todayCmd = command "today" (info (pure TimeToday) (progDesc "List today's time."))

startTimerCmd :: Mod CommandFields HoggleCmd
startTimerCmd = command "start" (info (pure StartTimer) (progDesc "Start a timer."))

stopTimerCmd :: Mod CommandFields HoggleCmd
stopTimerCmd = command "stop" (info (pure StopTimer) (progDesc "Stop the current timer."))

howLongCmd :: Mod CommandFields HoggleCmd
howLongCmd = command "howlong" (info (pure HowLong) (progDesc "How long until 8h per day reached."))

reportCmd :: Mod CommandFields HoggleCmd
reportCmd = command "report" (info (Report <$> strArgument (metavar "SINCE")
                                           <*> strArgument (metavar "UNTIL"))
                                   (progDesc "Request a report for the specified time range."))

infoCmd :: Mod CommandFields HoggleCmd
infoCmd = command "info" (info (pure Info) (progDesc "Display workspaces, clients and projects"))

hoggleArgsParser :: Parser HoggleArgs
hoggleArgsParser = HoggleArgs <$> token <*> lastDowOpt <*> workHoursOpt <*> subparser (todayCmd <> startTimerCmd <> stopTimerCmd <> howLongCmd <> reportCmd <> infoCmd)

startOfCurrentWeek :: IO Day
startOfCurrentWeek = do
  today <- utctDay <$> getCurrentTime
  let (year,weekNr,_) = toWeekDate today
      monday = fromWeekDate year weekNr 1
  return monday

die :: String -> IO ()
die msg = hPutStrLn stderr msg >> exitFailure

requiredTime :: Integer -> Integer -> IO Integer
requiredTime lastDow hoursPerDay = dowToSecondsNeeded
                                 . min lastDow
                                 . fromIntegral
                                 . thrd
                                 . toWeekDate
                                 . utctDay
                               <$> getCurrentTime
  where thrd (_,_,x) = x
        dowToSecondsNeeded dow = dow * hoursPerDay * 60 * 60
