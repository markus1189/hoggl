{-# LANGUAGE RecordWildCards #-}
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
                           ,Project(..)
                           ,ProjectId(..)
                           ,DetailedReport(..)
                           ,TogglApi
                           ,ToggleReportApi

                           ,parseTimeStamp) where

import           Codec.Binary.Base64.String (encode)
import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
import           Control.Monad.Fail
import           Data.Aeson (FromJSON(..), Value (..), (.:), (.:?), ToJSON(..), object, (.=), (.!=))
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as H
import           Data.Hashable (Hashable)
import           Data.Monoid ((<>))
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Calendar (Day)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM, iso8601DateFormat)
import           Servant.API
import           Servant.Client

newtype TimeEntryId = TID Integer deriving (Show,Eq,FromJSON,ToHttpApiData)
newtype WorkspaceId = WID Integer deriving (Show,Eq,FromJSON,ToHttpApiData)
newtype ProjectId = PID Integer deriving (Show,Eq,FromJSON,ToHttpApiData)
newtype ApiToken = ApiToken String deriving (IsString)
newtype ISO6801 = ISO6801 UTCTime deriving (Show,Eq,Ord)
newtype ISO6801Date = ISO6801Date Day deriving (Show,Eq,Ord)

instance ToHttpApiData ISO6801 where
   toUrlPiece (ISO6801 t) =
     toUrlPiece . addColon $ formatTime defaultTimeLocale "%Y-%m-%dT%H%::%M:%S%z" t

addColon :: String -> String
addColon s = reverse (p2 <> ":" <> su)
  where sr = reverse s
        p2 = take 2 sr
        su = drop 2 sr

instance FromJSON ISO6801 where
  parseJSON (String s) = ISO6801 <$> parseTimeStamp s
  parseJSON _ = mzero

parseTimeStamp :: (MonadFail m, Monad m) => Text -> m UTCTime
parseTimeStamp ts =
  parseTimeM True
             defaultTimeLocale
             (iso8601DateFormat (Just "%H:%M:%S%z"))
             (removeColon (T.unpack ts))
  where removeColon s =
          reverse (dropWhile (/= '+') (reverse s)) ++
            reverse (filter (/= ':') (takeWhile (/= '+') (reverse s)))


instance ToHttpApiData ISO6801Date where
   toUrlPiece (ISO6801Date day) = toUrlPiece (formatTime defaultTimeLocale "%Y-%m-%d" day)

data Token = Api String
           | UserPass String String
           deriving (Show,Eq)

instance ToHttpApiData Token where
   toUrlPiece (Api token) = toUrlPiece $ "Basic " ++ encode (token ++ ":api_token")
   toUrlPiece (UserPass user pass) = toUrlPiece $ "Basic " ++ encode (user ++ ":" ++ pass)

data HogglError = ServantError ClientError | HogglError String deriving Show

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
                           ,teStart :: ISO6801
                           ,teStop :: Maybe ISO6801
                           ,teDuration :: NominalDiffTime
                           ,teDescription :: Maybe Text
                           ,teTags :: Maybe [Text]
                           }deriving (Show,Eq)

instance FromJSON TimeEntry where
  parseJSON v@(Object o) = ((o .: "data") >>= p) <|> p v
    where p (Object d) = TimeEntry <$> d .: "id"
                                   <*> d .:?? "pid"
                                   <*> d .:?? "project"
                                   <*> d .:?? "client"
                                   <*> d .: "start"
                                   <*> (d .: "stop" <|> d.:?? "end")
                                   <*> (convert <$> ((d .: "duration") <|> ((`div` 1000) <$> (d .: "dur"))))
                                   <*> d .:? "description"
                                   <*> d .:? "tags"
          p _ = mzero
          convert :: Integer -> NominalDiffTime
          convert = fromIntegral
  parseJSON _ = mzero

(.:??) :: (FromJSON a, Hashable k, Eq k) => H.HashMap k Value -> k -> Parser (Maybe a)
obj .:?? key = case H.lookup key obj of
               Nothing -> pure Nothing
               Just Null -> pure Nothing
               Just v  -> Just <$> parseJSON v
{-# INLINE (.:??) #-}

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

data Project = Project { prId :: ProjectId
                       , prWsId :: WorkspaceId
                       , prName :: Text
                       , prBillable :: Bool
                       , prPrivate :: Bool
                       , prActive :: Bool
                       , prAt :: ISO6801
                       }

instance FromJSON Project where
  parseJSON (Object o) = Project <$> o .: "id"
                                 <*> o .: "wid"
                                 <*> o .: "name"
                                 <*> o .: "billable"
                                 <*> o .: "is_private"
                                 <*> o .: "active"
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
                                        <*> ((`div` 1000 ) <$> (o .: "total_grand"))
                                        <*> o .: "data"
  parseJSON _ = mzero

type WithAuth a = "api" :> "v8" :> Header "Authorization" Token :> a

type Current =        WithAuth ("time_entries" :> "current" :> Get '[JSON] TimeEntry)
type Stop =           WithAuth ("time_entries" :> Capture "time_entry_id" TimeEntryId :> "stop" :> Put '[JSON] TimeEntry)
type Start =          WithAuth ("time_entries" :> "start" :> ReqBody '[JSON] TimeEntryStart :> Post '[JSON] TimeEntry)
type Details =        WithAuth ("time_entries" :> Capture "time_entry_id" TimeEntryId :> Get '[JSON] TimeEntry)
type GetEntries =     WithAuth ("time_entries" :> QueryParam "start_date" ISO6801 :> QueryParam "end_date" ISO6801 :> Get '[JSON] [TimeEntry])
type ListWorkspaces = WithAuth ("workspaces" :> Get '[JSON] [Workspace])
type ListProjects =   WithAuth ("workspaces" :> Capture "workspace_id" WorkspaceId :> "projects" :> Get '[JSON] [Project])

type TogglApi = Current :<|> Stop :<|> Start :<|> Details :<|> GetEntries :<|> ListWorkspaces :<|> ListProjects

type GetDetailedReport = "reports" :> "api" :> "v2" :> "details" :> Header "Authorization" Token :> QueryParam "workspace_id" WorkspaceId :> QueryParam "since" ISO6801Date :> QueryParam "until" ISO6801Date :> QueryParam "user_agent" Text :> Get '[JSON] DetailedReport

type ToggleReportApi = GetDetailedReport
