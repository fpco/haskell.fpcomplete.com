{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Docs
  ( Docs (..)
  , Page (..)
  , PageHtml
  , getDocLoader
  ) where

import ClassyPrelude.Yesod hiding (Source, pageTitle, pageBody)
import System.FilePath
import CMarkGFM
import Text.Blaze.Html (preEscapedToHtml)
import Network.HTTP.Simple
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Text.Encoding (decodeUtf8')
import Data.Yaml
import Control.AutoUpdate
import Text.XML (Document (..), Node (..), Element (..))
import Text.XML.Cursor
import Text.HTML.DOM (parseSTChunks)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

data Page body = Page
  { pageTitle :: !Html
  , pageLastUpdated :: Day
  , pageDescription :: !(Maybe Text)
  , pageAuthor :: !(Maybe Text)
  , pageHead :: !Html
  , pageBody :: !body
  , pageSkipH1 :: !Bool
  , pageEditLink :: !(Maybe Text)
  , pageListed :: !Bool
  }

-- | Wrap in IO so that remote content can be downloaded on demand
type PageHtml = Page (IO Html)

-- | Body is URL containing the raw Markdown
instance body ~ Maybe String => FromJSON (Page body) where
  parseJSON = withObject "Page" $ \o -> do
    pageTitle <- (toHtml :: Text -> Html) <$> (o .: "title")
    pageLastUpdated <- o .: "last-updated"
    pageDescription <- o .:? "description"
    pageAuthor <- o .:? "author"
    pageHead <- (fmap (preEscapedToHtml :: Text -> Html) <$> (o .:? "head")) .!= ""
    pageSkipH1 <- o .:? "skip-h1" .!= False
    pageBody <- o .:? "url"
    pageListed <- o .:? "listed" .!= True
    let pageEditLink = Nothing
    pure Page {..}

data Docs = Docs
  { docsLibraries :: !(Map Text PageHtml)
  , docsTutorials :: !(Map Text PageHtml)
  , docsPages :: !(Map Text PageHtml)
  }

loadDocs :: IO Docs
loadDocs = Docs
  <$> loadDir "libraries"
  <*> loadDir "tutorials"
  <*> loadDir "pages"
  where
    loadDir subdir =
      runConduitRes $
      sourceDirectoryDeep True ("content" </> subdir) .|
      foldMapMC toDocMap

toDocMap
  :: MonadIO m
  => FilePath
  -> m (Map Text PageHtml)
toDocMap fp = liftIO $
  case splitExtension $ takeFileName fp of
    (name, ".md") -> byName name <$> getMarkdownDoc fp
    (name, ".yaml") -> byName name <$> getYamlDoc fp
    _ -> pure mempty
  where
    byName :: String -> PageHtml -> Map Text PageHtml
    byName name page = singletonMap (fromString name) page

renderMarkdown :: ByteString -> IO Html
renderMarkdown bodyBS = do
  bodyText <- either throwIO pure $ decodeUtf8' bodyBS
  pure $ addPermalinks $ commonmarkToHtml
    [ optSmart
    , optUnsafe
    ]
    [ extStrikethrough
    , extTable
    , extAutolink
    ]
    bodyText

addPermalinks :: Text -> Html
addPermalinks orig =
  case parseSTChunks [orig] of
    Document _ root _ ->
      case root of
        Element "html" _ nodes -> foldMap (toHtml . goNode) nodes
        _ -> toHtml $ goElem root
  where
    goElem (Element name attrs nodes) =
      Element name (addId name attrs nodes) $ map goNode nodes

    goNode (NodeElement e) = NodeElement $ goElem e
    goNode n = n

    isHeading name = name `Set.member` Set.fromList ["h1", "h2", "h3", "h4", "h5", "h6"]
    addId name attrs nodes
      | isHeading name && "id" `Map.notMember` attrs =
          let id' = slugify $ mconcat $ fromNode (NodeElement $ Element name attrs nodes) $// content
           in Map.insert "id" id' attrs
      | otherwise = attrs

    slugify = T.intercalate "-" . T.words . T.map toSpace . T.toLower

    toSpace c
      | 'a' <= c && c <= 'z' = c
      | '0' <= c && c <= '9' = c
      | otherwise = c

getMarkdownDoc :: FilePath -> IO PageHtml
getMarkdownDoc fp = handleAny onErr $ do
  bs <- B.readFile fp
  (frontmatterBS, bodyBS) <- maybe (error "No frontmatter found") pure $ do
    "---":ls <- Just $ B8.lines bs
    (fm, "---":body) <- Just $ break (== "---") ls
    Just (B8.unlines fm, B8.unlines body)
  page0 <- decodeThrow frontmatterBS
  for_ (pageBody page0) $ \url ->
    error $ "Should not have a URL: " ++ show url
  body <- renderMarkdown bodyBS
  pure page0
    { pageBody = pure body
    , pageEditLink =
        Just $
        "https://github.com/fpco/haskell.fpcomplete.com/blob/master/" <>
        fromString fp
    }
  where
    onErr e = error $ concat
      [ "Couldn't load Markdown page "
      , show fp
      , ": "
      , show e
      ]

getYamlDoc :: FilePath -> IO PageHtml
getYamlDoc fp = handleAny onErr $ do
  page0 <- decodeFileThrow fp
  req <-
    case pageBody page0 of
      Nothing -> error "Must provide a URL"
      Just url -> parseRequestThrow url
  getter <- mkAutoUpdate defaultUpdateSettings
    { updateFreq = 5 * 60 * 1000 * 1000
    , updateAction =
        handleAny onErr $
        httpBS req >>=
        renderMarkdown . dropFirstHeading . getResponseBody
    }
  pure page0
    { pageBody = getter
    }
  where
    onErr :: SomeException -> IO a
    onErr e = error $ concat
      [ "Couldn't load YAML page "
      , show fp
      , ": "
      , show e
      ]

    dropFirstHeading bs
      | "#" `B.isPrefixOf` bs = B.drop 1 $ B.dropWhile (/= 10) bs
      | otherwise = bs

getDocLoader
  :: Bool -- ^ dev mode: reload on each page load
  -> IO (IO Docs)
getDocLoader True = pure loadDocs
getDocLoader False = do
  docs <- loadDocs
  pure $ pure docs
