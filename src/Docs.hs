{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Docs
  ( Docs (..)
  , Doc (..)
  , getDocLoader
  ) where

import ClassyPrelude.Yesod hiding (Source)
import System.FilePath
import CMarkGFM
import Text.Blaze.Html (preEscapedToHtml)
import qualified Data.Text as T
import Network.HTTP.Simple
import System.Mem.Weak (deRefWeak)
import Control.Concurrent (threadDelay)
import Data.Function (fix)
import Text.HTML.DOM (parseSTChunks)
import Text.XML.Cursor (fromDocument, ($//), element, content, (&/))

data Docs = Docs
  { docsLibraries :: !(Map Text Doc)
  , docsTutorials :: !(Map Text Doc)
  }
instance Semigroup Docs where
  Docs x1 x2 <> Docs y1 y2 = Docs (x1 <> y1) (x2 <> y2)
instance Monoid Docs where
  mempty = Docs mempty mempty

data Doc = Doc
  { docTitle :: !Html
  , docBody :: !Html
  , docEditLink :: !(Maybe Text)
  }

data Source = HaskellLang | Local

loadDocs
  :: Bool -- ^ dev mode
  -> IO Docs
loadDocs devMode =
         runConduitRes
       $ sourceDirectoryDeep True "tutorials"
      .| foldMapMC (liftIO . toDocs devMode Local)

toDocs
  :: Bool -- ^ dev mode
  -> Source -> FilePath -> IO Docs
toDocs devMode src fp =
  case splitExtension $ takeFileName fp of
    (name, ".md") -> byName name <$> getMarkdownDoc src fp
    (name, ".url")
      | devMode -> pure mempty
      | otherwise -> byName name <$> getUrlDoc fp
    _ -> pure mempty
  where
    byName (fromString -> name) doc =
      case stripPrefix "package-" name of
        Just lib ->
          let doc' = doc { docTitle = docTitle doc <> " - the " <> toHtml lib <> " library" }
           in Docs (singletonMap lib doc') mempty
        Nothing -> Docs mempty (singletonMap name doc)

getMarkdownDoc :: Source -> FilePath -> IO Doc
getMarkdownDoc src fp = do
  markdownText <- readFileUtf8 fp
  let htmlText = commonmarkToHtml
        [optSmart, optUnsafe]
        [extStrikethrough, extTable, extAutolink]
        markdownText
  pure Doc
    { docTitle = extractH1 fp htmlText
    , docBody = preEscapedToHtml htmlText
    , docEditLink = Just $ mconcat
        [ sourcePrefix src
        , pack $ takeFileName fp
        ]
    }

extractH1 :: FilePath -> Text -> Html
extractH1 fp t =
  case mconcat $ fromDocument (parseSTChunks [t]) $// element "h1" &/ content of
    "" -> error $ "No title found for: " ++ fp
    x -> toHtml x

sourcePrefix :: Source -> Text
sourcePrefix HaskellLang =
  "https://github.com/haskell-lang/haskell-lang/blob/master/static/tutorial/"
sourcePrefix Local =
  "https://github.com/fpco/haskell.fpcomplete.com/blob/master/tutorials/"

getUrlDoc :: FilePath -> IO Doc
getUrlDoc fp = do
  text <- readFileUtf8 fp
  case T.lines text of
    [title, url] -> do
      req <- parseRequest $ T.unpack url
      markdownText <- fmap mconcat $ httpSink req $ const $ decodeUtf8C .| sinkList
      let htmlText = commonmarkToHtml
            [optSmart, optUnsafe]
            [extStrikethrough, extTable, extAutolink]
            markdownText
      pure Doc
        { docTitle = toHtml title
        , docBody = preEscapedToHtml htmlText
        , docEditLink = Nothing
        }
    _ -> error $ "Malformed file: " ++ show fp

getDocLoader
  :: Bool -- ^ dev mode: reload, and no URLs
  -> IO (IO Docs)
getDocLoader True = pure $ loadDocs True
getDocLoader False = do
  docs0 <- loadDocs False
  ref0 <- newIORef docs0
  weak <- mkWeakIORef ref0 (pure ())
  void $ async $ fix $ \loop -> do
    threadDelay $ 60 * 5 * 1000 * 1000
    mref <- deRefWeak weak
    case mref of
      Nothing -> pure ()
      Just ref -> do
        edocs <- tryAny $ loadDocs False
        case edocs of
          Left e -> print e -- FIXME proper logging
          Right docs -> writeIORef ref docs
        loop
  pure $ readIORef ref0
