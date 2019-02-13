{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Import.NoFoundation
    ( module Import
    , OS (..)
    ) where

import ClassyPrelude.Yesod   as Import hiding (PageContent (..))
import Docs                  as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import

import qualified Data.Map.Strict as Map
import Text.Blaze (ToMarkup (..))

data OS = OSX | Windows | Linux
  deriving (Show, Eq, Read, Enum, Bounded)
instance PathPiece OS where
  toPathPiece OSX = "osx"
  toPathPiece Windows = "windows"
  toPathPiece Linux = "linux"

  fromPathPiece = (`Map.lookup` osMap)
instance ToMarkup OS where
  toMarkup OSX = "OS X"
  toMarkup Windows = "Windows"
  toMarkup Linux = "Linux"

osMap :: Map Text OS
osMap = Map.fromList $ map (\os -> (toPathPiece os, os)) [minBound..maxBound]
