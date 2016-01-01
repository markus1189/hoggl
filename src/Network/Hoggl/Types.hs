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
                           ,TogglApi) where

import Codec.Binary.Base64.String (encode)
import Control.Monad (mzero, mplus)
import Data.Aeson (FromJSON(..), Value (..), (.:), (.:?), ToJSON(..), object, (.=))
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, NominalDiffTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Servant.API
import Servant.Client

newtype TimeEntryId = TID Integer deriving (Show,Eq,FromJSON,ToText)
newtype ApiToken = ApiToken String deriving (IsString)

newtype ISO6801 = ISO6801 UTCTime

instance ToText ISO6801 where
  toText (ISO6801 t) =
    toText . addColon $ formatTime defaultTimeLocale "%Y-%m-%dT%H%::%M:%S%z" t

addColon :: String -> String
addColon s = reverse (p2 <> ":" <> su)
  where sr = reverse s
        p2 = take 2 sr
        su = drop 2 sr

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
                           ,teStart :: Text
                           ,teStop :: Maybe Text
                           ,teDuration :: NominalDiffTime
                           ,teDescription :: Maybe Text
                           }deriving (Show,Eq)

instance FromJSON TimeEntry where
  parseJSON v@(Object o) = p v `mplus` ((o .: "data") >>= p)
    where p (Object d) = TimeEntry <$> d .: "id"
                                   <*> d .: "start"
                                   <*> d .:? "stop"
                                   <*> (convert <$> (d .: "duration"))
                                   <*> d .:? "description"
          p _ = mzero
          convert :: Integer -> NominalDiffTime
          convert = fromIntegral
  parseJSON _ = mzero

type Current = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> "current" :> Get '[JSON] TimeEntry
type Stop = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> Capture "time_entry_id" TimeEntryId :> "stop" :> Put '[JSON] TimeEntry
type Start = "api" :> "v8" :> "time_entries" :> "start" :> Header "Authorization" Token :> ReqBody '[JSON] TimeEntryStart :> Post '[JSON] TimeEntry
type Details = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> Capture "time_entry_id" TimeEntryId :> Get '[JSON] TimeEntry
type GetEntries = "api" :> "v8" :> "time_entries" :> Header "Authorization" Token :> QueryParam "start_date" ISO6801 :> QueryParam "end_date" ISO6801 :> Get '[JSON] [TimeEntry]

type TogglApi = Current :<|> Stop :<|> Start :<|> Details :<|> GetEntries
