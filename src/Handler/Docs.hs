{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Docs
  ( getLearnR
  , getTutorialR
  , getLibraryR
  ) where

import Import
import qualified Data.Map.Strict as Map

getDocs :: Handler Docs
getDocs = getYesod >>= liftIO . appDocs

getLearnR :: Handler Html
getLearnR = undefined

getTutorialR :: Text -> Handler Html
getTutorialR = helper docsTutorials

getLibraryR :: Text -> Handler Html
getLibraryR = helper docsLibraries

helper :: (Docs -> Map Text Doc) -> Text -> Handler Html
helper getter name = do
  docs <- getDocs
  doc <- maybe notFound pure
       $ Map.lookup name
       $ getter docs
  defaultLayout $ do
    setTitle $ docTitle doc
    [whamlet|
      <p>
        <a href=@{LearnR}>Return to Learn
        $maybe edit <- docEditLink doc
          |
          <a href=#{edit}>Edit on Github
    |]
    toWidget $ docBody doc
