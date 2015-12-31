{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Codec.Binary.Base64.String (encode)
import           Control.Concurrent (threadDelay)
import           Control.Monad (mzero, forever, (<=<))
import           Control.Monad.Trans.Either (EitherT(..), runEitherT)
import           Data.Aeson (FromJSON(..), Value (..), (.:), (.:?), ToJSON(..), object, (.=))
import           Data.Bifunctor (first)
import           Data.Foldable (traverse_)
import           Data.Proxy (Proxy(Proxy))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock (UTCTime, NominalDiffTime, getCurrentTime, diffUTCTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Data.Time.Format (parseTimeM, defaultTimeLocale, iso8601DateFormat)
import           Formatting (sformat, (%))
import           Formatting.Time (hours,minutes)
import           Servant.API
import           Servant.Client

data Errors = ServantError ServantError | CustomError String deriving Show

newtype TimeEntryId = TID Integer deriving (Show,Eq,FromJSON,ToText)

data TimeEntryStart = TES { tesDescription :: Maybe Text
                          , tesTags :: [Text]
                          , tesPid :: Maybe Integer
                          , tesCreatedWith :: Text
                          } deriving (Show,Eq)

instance ToJSON TimeEntryStart where
  toJSON (TES desc tags pid createdWith) = object ["time_entry" .= object ["description" .= desc
                                                                          ,"tags" .= tags
                                                                          ,"pid" .= pid
                                                                          ,"created_with" .= createdWith]]

data TimeEntry = TimeEntry {teId :: TimeEntryId
                           ,teStart :: Text
                           ,teStop :: Maybe Text
                           ,teDuration :: Integer
                           ,teDescription :: Maybe Text
                           }deriving (Show,Eq)

showDuration :: TimeEntry -> IO NominalDiffTime
showDuration te = do
  posix <- getPOSIXTime
  let dur = posix + fromInteger (teDuration te)
  return dur

instance FromJSON TimeEntry where
  parseJSON (Object o) = (o .: "data") >>= p
    where p (Object d) = TimeEntry <$> d .: "id"
                                   <*> d .: "start"
                                   <*> d .:? "stop"
                                   <*> d .: "duration"
                                   <*> d .:? "description"
          p _ = mzero
  parseJSON _ = mzero

newtype Token = Token Text deriving (Show,Eq,ToText)
newtype ApiToken = ApiToken String deriving (IsString)

data Origin = Origin deriving (Show,Eq)

instance FromJSON Origin where
  parseJSON _ = return Origin

mkToken :: String -> Token
mkToken token = Token . T.pack $ "Basic " ++ encode (token ++ ":api_token")

type Current = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> "current" :> Get '[JSON] TimeEntry
type Stop = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> Capture "time_entry_id" TimeEntryId :> "stop" :> Put '[JSON] TimeEntry
type Start = "api" :> "v8" :> "time_entries" :> "start" :> Header "Authorization" Token :> ReqBody '[JSON] TimeEntryStart :> Post '[JSON] TimeEntry
type Details = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> Capture "time_entry_id" TimeEntryId :> Get '[JSON] TimeEntry

type TogglAPI = Current :<|> Stop :<|> Start :<|> Details

togglAPI :: Proxy TogglAPI
togglAPI = Proxy

currentTimeEntry' :: Maybe Token -> EitherT ServantError IO TimeEntry
stopTimer :: Maybe Token -> TimeEntryId -> EitherT ServantError IO TimeEntry
startTimer :: Maybe Token -> TimeEntryStart -> EitherT ServantError IO TimeEntry
getTimer :: Maybe Token -> TimeEntryId -> EitherT ServantError IO TimeEntry
(currentTimeEntry' :<|> stopTimer :<|> startTimer :<|> getTimer) =
  client togglAPI (BaseUrl Https "toggl.com" 443)

currentTimeEntry :: Maybe Token -> EitherT ServantError IO (Maybe TimeEntry)
currentTimeEntry mtoken = EitherT $ do
  res <- runEitherT (currentTimeEntry' mtoken)
  case res of
    Left DecodeFailure {responseBody = "{\"data\":null}"} -> return (Right Nothing)
    Left e -> return (Left e)
    Right te -> return (Right (Just te))

defaultTimeEntry :: TimeEntryStart
defaultTimeEntry = TES {tesDescription = Nothing
                  ,tesTags = []
                  ,tesPid = Nothing
                  ,tesCreatedWith = "hoggl"
                  }

loopCurrentDuration :: Token -> IO ()
loopCurrentDuration authorization = forever $ do
  r <- runEitherT (currentTimeEntry (Just authorization))
  either (const $ return ()) (traverse_ (print <=< showDuration)) r
  threadDelay (1000 * 1000)

main :: IO ()
main = return ()

exampleEntry :: TimeEntry
exampleEntry = TimeEntry {teId = TID 315969512
                         ,teStart = "2015-12-31T09:45:54+00:00"
                         ,teStop = Nothing
                         ,teDuration = -1451555154
                         ,teDescription = Nothing}

exampleStopped :: TimeEntry
exampleStopped = TimeEntry {teId = TID 315969512
                           ,teStart = "2015-12-31T09:45:54+00:00"
                           ,teStop = Just "2015-12-31T10:21:30+00:00"
                           ,teDuration = 2136
                           ,teDescription = Nothing
                           }

parseStart :: TimeEntry -> IO UTCTime
parseStart TimeEntry { teStart = s } =
  parseTimeM True
             defaultTimeLocale
             (iso8601DateFormat (Just "%H:%M:%S+00:00"))
             (T.unpack s)

calcDuration :: TimeEntry -> IO NominalDiffTime
calcDuration te = do
  start <- parseStart te
  curr <- getCurrentTime
  return (diffUTCTime curr start)

loopOffline :: Token -> TimeEntry -> IO ()
loopOffline authorization te = sequence_ . cycle $ replicate 5 act1 ++ [act2]
  where act1 = calcDuration te >>= T.putStrLn . pretty >> threadDelay (1000*1000)
        act2 = runEitherT (currentTimeEntry (Just authorization)) >>= either print print

pretty :: RealFrac n => n -> Text
pretty n = sformat (hours 0 % "h " % minutes 0 % "m") n (n/60)

tryStartDefault :: Token -> IO (Either Errors TimeEntry)
tryStartDefault authorization = do
  currentTimer <- runEitherT (currentTimeEntry (Just authorization))
  case currentTimer of
    Right Nothing ->
      first ServantError <$> runEitherT (startTimer (Just authorization) defaultTimeEntry)
    Right (Just _) -> return (Left (CustomError "There already is a running timer!"))
    Left e -> return (Left (ServantError e))

tryStopRunning :: Token -> IO (Either Errors TimeEntry)
tryStopRunning authorization = do
  currentTimer <- runEitherT (currentTimeEntry (Just authorization))
  case currentTimer of
    Right (Just TimeEntry {teId = tid}) ->
      first ServantError <$> runEitherT (stopTimer (Just authorization) tid)
    Right Nothing -> return (Left (CustomError "No timer running!"))
    Left e -> return (Left (ServantError e))
