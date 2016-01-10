{-# LANGUAGE RecordWildCards #-}
module Network.Hoggl.Pretty (workspacePretty) where

import Data.Monoid ((<>))
import qualified Data.Text as T

import Network.Hoggl.Types

workspacePretty :: Workspace -> String
workspacePretty Workspace {..} = show wid <> " (" <> T.unpack wsName <> ")"
  where (WID wid) = wsId
