{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Yaml
import Text.Blaze.Html (preEscapedToHtml)
import CMarkGFM
import Data.Text.Encoding (decodeUtf8')

newtype MakePage = MakePage (Html -> Page)

instance FromJSON MakePage where
  parseJSON = withObject "MakePage" $ \o -> do
    pageTitle <- (toHtml :: Text -> Html) <$> (o .: "title")
    pageDescription <- o .:? "description"
    pageAuthor <- o .:? "author"
    pageHead <- (fmap (preEscapedToHtml :: Text -> Html) <$> (o .:? "head")) .!= ""
    pageSkipH1 <- o .:? "skip-h1" .!= False
    pure $ MakePage $ \pageBody -> Page {..}

displayMarkdown :: FilePath -> Handler Html
displayMarkdown base = do
  let fp = "pages" </> base <.> "md"
  bs <- liftIO $ B.readFile fp
  (frontmatterBS, bodyBS) <- maybe (error "Invalid Markdown input") pure $ do
    "---":ls <- Just $ B8.lines bs
    (fm, "---":body) <- Just $ break (== "---") ls
    Just (B8.unlines fm, B8.unlines body)
  MakePage makePage <- decodeThrow frontmatterBS
  bodyText <- either throwIO pure $ decodeUtf8' bodyBS
  displayPage $ makePage $ preEscapedToHtml $ commonmarkToHtml
    [ optSmart
    , optUnsafe
    ]
    [ extStrikethrough
    , extTable
    , extAutolink
    ]
    bodyText

getDocs :: Handler Docs
getDocs = getYesod >>= liftIO . appDocs
