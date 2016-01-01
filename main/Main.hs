module Main where

import           Control.Monad.Trans.Either (runEitherT)
import qualified Data.Text.IO as T
import           Data.Time.Calendar (Day)
import           Data.Time.Calendar.WeekDate (toWeekDate, fromWeekDate)
import           Data.Time.Clock (UTCTime(..), getCurrentTime)
import           GHC.IO.Handle.FD (stderr)
import           Options.Applicative
import           System.Exit (exitFailure)
import           System.IO (hPutStrLn)

import           Network.Hoggl
import           Network.Hoggl.Types

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

run (HoggleArgs auth lastDow workHours HowLong) = do
  start <- startOfCurrentWeek
  eCurLogged <- runEitherT (timeEntriesFromTillNow auth start)
  case eCurLogged of
    Left _ -> die "Failed to get time entries."
    Right ts -> do
      worked <- sum <$> traverse calcDuration ts
      req <- requiredTime lastDow workHours
      let diff = worked - fromIntegral req
      T.putStrLn (pretty diff)

data HoggleArgs = HoggleArgs Token Integer Integer HoggleCmd
data HoggleCmd = TimeToday | StartTimer | StopTimer | HowLong

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

hoggleArgsParser :: Parser HoggleArgs
hoggleArgsParser = HoggleArgs <$> token <*> lastDowOpt <*> workHoursOpt <*> subparser (todayCmd <> startTimerCmd <> stopTimerCmd <> howLongCmd)

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
