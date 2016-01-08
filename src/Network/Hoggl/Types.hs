{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Network.Hoggl.Types (TimeEntryId(..)
                           ,Token(..)
                           ,HogglError(..)
                           ,TimeEntryStart(..)
                           ,TimeEntry(..)
                           ,ISO6801(..)
                           ,ISO6801Date(..)
                           ,Workspace(..)
                           ,WorkspaceId(..)
                           ,ProjectId(..)
                           ,DetailedReport(..)
                           ,TogglApi
                           ,ToggleReportApi) where

import           Codec.Binary.Base64.String (encode)
import           Control.Applicative ((<|>))
import           Control.Monad (mzero, mplus)
import           Data.Aeson (FromJSON(..), Value (..), (.:), (.:?), ToJSON(..), object, (.=), (.!=))
import           Data.Monoid ((<>))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Data.Time.Format (parseTimeM, iso8601DateFormat)
import           Servant.API
import           Servant.Client

newtype TimeEntryId = TID Integer deriving (Show,Eq,FromJSON,ToText)
newtype WorkspaceId = WID Integer deriving (Show,Eq,FromJSON,ToText)
newtype ProjectId = PID Integer deriving (Show,Eq,FromJSON,ToText)
newtype ApiToken = ApiToken String deriving (IsString)
newtype ISO6801 = ISO6801 UTCTime deriving (Show,Eq)
newtype ISO6801Date = ISO6801Date Day deriving (Show,Eq)

instance ToText ISO6801 where
  toText (ISO6801 t) =
    toText . addColon $ formatTime defaultTimeLocale "%Y-%m-%dT%H%::%M:%S%z" t

addColon :: String -> String
addColon s = reverse (p2 <> ":" <> su)
  where sr = reverse s
        p2 = take 2 sr
        su = drop 2 sr

instance FromJSON ISO6801 where
  parseJSON (String s) = ISO6801 <$> parseTimeStamp s
  parseJSON _ = mzero

parseTimeStamp :: Monad m => Text -> m UTCTime
parseTimeStamp ts = parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S+00:00")) (T.unpack ts)

instance ToText ISO6801Date where
  toText (ISO6801Date day) = toText (formatTime defaultTimeLocale "%Y-%m-%d" day)

data Token = Api String
           | UserPass String String
           deriving (Show,Eq)

instance ToText Token where
  toText (Api token) = toText $ "Basic " ++ encode (token ++ ":api_token")
  toText (UserPass user pass) = toText $ "Basic " ++ encode (user ++ ":" ++ pass)

data HogglError = ServantError ServantError | HogglError String deriving Show

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
                           ,teProjectId :: Maybe ProjectId
                           ,teProject :: Maybe Text
                           ,teClient :: Maybe Text
                           ,teStart :: Text
                           ,teStop :: Maybe Text
                           ,teDuration :: NominalDiffTime
                           ,teDescription :: Maybe Text
                           }deriving (Show,Eq)

instance FromJSON TimeEntry where
  parseJSON v@(Object o) = p v `mplus` ((o .: "data") >>= p)
    where p (Object d) = TimeEntry <$> d .: "id"
                                   <*> d .: "pid"
                                   <*> d .: "project"
                                   <*> d .: "client"
                                   <*> d .: "start"
                                   <*> d .:? "stop"
                                   <*> (convert <$> ((d .: "duration") <|> (d .: "dur")))
                                   <*> d .:? "description"
          p _ = mzero
          convert :: Integer -> NominalDiffTime
          convert = fromIntegral
  parseJSON _ = mzero

data Workspace = Workspace {wsId :: WorkspaceId
                           ,wsName :: Text
                           ,wsPremium :: Bool
                           ,wsAdmin :: Bool
                           ,wsDefaultHourlyRate :: Double
                           ,wsDefaultCurrency :: Text
                           ,wsRounding :: Int
                           ,wsRoundingMinutes :: Int
                           ,wsAt :: ISO6801
                           } deriving (Show,Eq)

instance FromJSON Workspace where
  parseJSON (Object o) = Workspace <$> o .: "id"
                                   <*> o .: "name"
                                   <*> o .: "premium"
                                   <*> o .: "admin"
                                   <*> o .: "default_hourly_rate"
                                   <*> o .: "default_currency"
                                   <*> o .: "rounding"
                                   <*> o .: "rounding_minutes"
                                   <*> o .: "at"
  parseJSON _ = mzero

data DetailedReport = DetailedReport {drPerPage :: Int
                                     ,drTotalCount :: Int
                                     ,drTotalBillable :: Double
                                     ,drTotalGrand :: Integer
                                     ,drData :: [TimeEntry]
                                     } deriving (Show,Eq)

instance FromJSON DetailedReport where
  parseJSON (Object o) = DetailedReport <$> o .: "per_page"
                                        <*> o .: "total_count"
                                        <*> o .:? "total_billable" .!= 0
                                        <*> o .: "total_grand"
                                        <*> o .: "data"
  parseJSON _ = mzero

type Current = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> "current" :> Get '[JSON] TimeEntry
type Stop = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> Capture "time_entry_id" TimeEntryId :> "stop" :> Put '[JSON] TimeEntry
type Start = "api" :> "v8" :> "time_entries" :> "start" :> Header "Authorization" Token :> ReqBody '[JSON] TimeEntryStart :> Post '[JSON] TimeEntry
type Details = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> Capture "time_entry_id" TimeEntryId :> Get '[JSON] TimeEntry
type GetEntries = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> QueryParam "start_date" ISO6801 :> QueryParam "end_date" ISO6801 :> Get '[JSON] [TimeEntry]
type ListWorkspaces = "api" :> "v8" :> "workspaces" :> Header "Authorization" Token :> Get '[JSON] [Workspace]

type TogglApi = Current :<|> Stop :<|> Start :<|> Details :<|> GetEntries :<|> ListWorkspaces

type GetDetailedReport = "reports" :> "api" :> "v2" :> "details" :> Header "Authorization" Token :> QueryParam "workspace_id" WorkspaceId :> QueryParam "since" ISO6801Date :> QueryParam "until" ISO6801Date :> QueryParam "user_agent" Text :> Get '[JSON] DetailedReport

type ToggleReportApi = GetDetailedReport
