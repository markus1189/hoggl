module Main where

import           Control.Monad.Trans.Either (runEitherT)
import qualified Data.Text.IO as T
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
run (HoggleArgs auth TimeToday) = do
  e <- runEitherT (timeEntriesToday auth)
  case e of
    Left _ -> do
      hPutStrLn stderr "There was an error."
      exitFailure
    Right ts -> do
      ds <- traverse calcDuration ts
      T.putStrLn (pretty (sum ds))

run (HoggleArgs auth StartTimer) = do
  e <- tryStartDefault auth
  case e of
    Left _ -> do
      hPutStrLn stderr "Failed to start timer."
      exitFailure
    Right _ -> return ()

run (HoggleArgs auth StopTimer) = do
  e <- tryStopRunning auth
  case e of
    Left _ -> do
      hPutStrLn stderr "Failed to stop timer."
      exitFailure
    Right _ -> return ()

data HoggleArgs = HoggleArgs Token HoggleCmd
data HoggleCmd = TimeToday | StartTimer | StopTimer

token :: Parser Token
token = Api <$> strOption (long "token" <> help "API Token")

todayCmd :: Mod CommandFields HoggleCmd
todayCmd = command "today" (info (pure TimeToday) (progDesc "List today's time."))

startTimerCmd :: Mod CommandFields HoggleCmd
startTimerCmd = command "start" (info (pure StartTimer) (progDesc "Start a timer."))

stopTimerCmd :: Mod CommandFields HoggleCmd
stopTimerCmd = command "stop" (info (pure StopTimer) (progDesc "Stop the current timer."))

hoggleArgsParser :: Parser HoggleArgs
hoggleArgsParser = HoggleArgs <$> token <*> subparser (todayCmd <> startTimerCmd <> stopTimerCmd)
