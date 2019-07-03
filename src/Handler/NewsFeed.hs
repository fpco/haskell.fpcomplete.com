{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.NewsFeed
  ( getRssR
  , getAtomR
  ) where

import Import
import Yesod.AtomFeed
import Yesod.RssFeed

import Data.List as List (maximum)
import qualified Data.Text.Lazy as LT
import Text.Blaze.Html.Renderer.Text as Text

import qualified Data.Map as M

getRssR :: Handler RepRss
getRssR = feedHelper rssFeed

getAtomR :: Handler RepAtom
getAtomR = feedHelper atomFeed

feedHelper :: (Feed (Route App) -> Handler a) -> Handler a
feedHelper func = do
  docs <- getDocs
  tups <- liftIO $ loadAllDocsPure docs
  func $ feed tups

data LoadedFeedDocs =
  LoadedFeedDocs
    { ldocsLibraries :: !(Map Text (Page Html))
    , ldocsTutorials :: !(Map Text (Page Html))
    }

loadAllDocsPure :: Docs -> IO LoadedFeedDocs
loadAllDocsPure Docs {..} = do
  ldocsLibraries <- loadMap docsLibraries
  ldocsTutorials <- loadMap docsTutorials
  pure LoadedFeedDocs {..}
  where
    loadMap :: Map Text PageHtml -> IO (Map Text (Page Html))
    loadMap = traverse go
    go :: PageHtml -> IO (Page Html)
    go p@Page {..} = do
      body <- pageBody
      pure $ p {pageBody = body}

feed :: LoadedFeedDocs -> Feed (Route App)
feed ds =
  let entries = makeEntries ds
   in Feed
        { feedTitle = "FP Complete  Haskell"
        , feedLinkSelf = RssR
        , feedLinkHome = HomeR
        , feedAuthor = "Tom Sydney Kerckhove <syd@fpcomplete.com"
        , feedDescription =
            "FP Complete is the leading provider of commercial Haskell tools and services"
        , feedLanguage = "en-us"
        , feedUpdated = List.maximum $ map feedEntryUpdated entries
        , feedLogo = Just (StaticR img_fplogo_svg, "FP Complete Logo")
        , feedEntries = entries
        }

makeEntries :: LoadedFeedDocs -> [FeedEntry (Route App)]
makeEntries LoadedFeedDocs {..} =
  sortOn (Down . feedEntryUpdated) $
  let makePageEntriesWithRoute rf = map (uncurry makePageEntry . first rf) . M.toList
   in concat
        [ makePageEntriesWithRoute LibraryR ldocsLibraries
        , makePageEntriesWithRoute TutorialR ldocsTutorials
        ]

makePageEntry :: Route App -> Page Html -> FeedEntry (Route App)
makePageEntry r Page {..} =
  FeedEntry
    { feedEntryLink = r
    , feedEntryUpdated = undefined
    , feedEntryTitle = LT.toStrict $ Text.renderHtml pageTitle
    , feedEntryContent = pageBody
    , feedEntryEnclosure = Nothing
    }
