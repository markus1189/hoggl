module Main (main) where

import Control.Concurrent (threadDelay)
import System.Environment (getArgs)

import Network.Hoggl
import Network.Hoggl.Types

main :: IO ()
main = do
  args <- getArgs
  let authorization = Api (head args)
  _ <- tryStartDefault authorization
  prettyCurrent authorization
  threadDelay (10 * 1000 * 1000)
  prettyCurrent authorization
  r <- tryStopRunning authorization
  print r
