{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import qualified Data.Map.Strict as Map

docHelper :: (Docs -> Map Text PageHtml) -> Text -> Handler Html
docHelper getter name = do
  docs <- getDocs
  page <- maybe notFound pure
       $ Map.lookup name
       $ getter docs
  displayPage page

getDocs :: Handler Docs
getDocs = getYesod >>= liftIO . appDocs
