{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Foundation where

import Import.NoFoundation
import Text.Hamlet                 (hamletFile)
import Yesod.Core.Types            (Logger)
import qualified ClassyPrelude.Yesod as Y
import Yesod.GitRev

data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appLogger      :: Logger
    , appDocs        :: !(IO Docs)
    , appGitRev      :: !GitRev
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = pure Nothing

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        pc <- widgetToPageContent widget
        render <- getUrlRenderParams
        displayPage Page
              { pageTitle = Y.pageTitle pc
              , pageDescription = Nothing
              , pageAuthor = Nothing
              , pageHead = Y.pageHead pc render
              , pageBody = Y.pageBody pc render
              , pageSkipH1 = True
              }

data Page = Page
  { pageTitle :: !Html
  , pageDescription :: !(Maybe Text)
  , pageAuthor :: !(Maybe Text)
  , pageHead :: !Html
  , pageBody :: !Html
  , pageSkipH1 :: !Bool
  }

displayPage :: Page -> Handler Html
displayPage Page {..} = do
  (fromInteger -> year, _, _) <- (toGregorian . utctDay) <$> liftIO getCurrentTime
  let firstYear = 2019 :: Int
      copyrightYears
        | year == firstYear = toHtml year
        | otherwise = toHtml firstYear <> "—" <> toHtml year
  withUrlRenderer $(hamletFile "templates/default-layout.hamlet")
