{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Sitemap
  ( getRobotsR
  , getSitemapR
  ) where

import Import
import Yesod.Sitemap
import qualified Data.Map.Strict as Map

getRobotsR :: Handler Text
getRobotsR = robots SitemapR

getSitemapR :: Handler TypedContent
getSitemapR = do
  docs <- getDocs
  sitemap $ do
    yield $ SitemapUrl HomeR Nothing Nothing (Just 1)
    yield $ SitemapUrl ContributeR Nothing Nothing (Just 0.5)
    yield $ SitemapUrl PromoteR Nothing Nothing (Just 0.8)
    yield $ SitemapUrl SuccessR Nothing Nothing (Just 0.9)
    yield $ SitemapUrl CommunityR Nothing Nothing (Just 0.5)
    yield $ SitemapUrl AboutR Nothing Nothing (Just 0.9)
    yield $ SitemapUrl LearnR Nothing Nothing (Just 0.7)
    yield $ SitemapUrl PhilosophyR Nothing Nothing (Just 0.7)
    yield $ SitemapUrl GetStartedR Nothing Nothing (Just 0.7)
    for_ [minBound..maxBound] $ \os ->
      yield $ SitemapUrl (GetStartedOSR os) Nothing Nothing (Just 0.3)

    for_ (Map.keys $ docsLibraries docs) $ \name ->
      yield $ SitemapUrl (LibraryR name) Nothing Nothing (Just 0.5)
    for_ (Map.keys $ docsTutorials docs) $ \name ->
      yield $ SitemapUrl (TutorialR name) Nothing Nothing (Just 0.5)
