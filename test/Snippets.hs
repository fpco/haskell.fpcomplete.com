{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PackageImports #-}
module Snippets where

import RIO
import RIO.Process
import RIO.FilePath (takeExtension, replaceExtension, (</>), (<.>))
import RIO.Directory (createDirectoryIfMissing, removeFile, doesFileExist)
import Conduit
import CMarkGFM
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Text.HTML.DOM as DOM
import Text.XML (Document (..), Element (..), Node (..))
import Control.Monad.Writer.Strict (execWriter, tell)
import qualified RIO.HashSet as HS
import qualified RIO.Map as Map
import qualified Crypto.Hash

check :: IO ()
check = runSimpleApp run

splitFiles :: LByteString -> RIO SimpleApp [FilePath]
splitFiles lbs
  | BL.null lbs = pure []
  | otherwise = do
      let (next, rest) = BL.break (== 0) lbs
      fp <- either throwIO pure $ decodeUtf8' $ BL.toStrict next
      rest' <- splitFiles $ BL.drop 1 rest
      pure $ T.unpack fp : rest'

data Content = Content
  { _snippets :: !(Map Text FilePath)
  }
  deriving Show
instance Semigroup Content where
  Content a <> Content x = Content (a <> x)
instance Monoid Content where
  mempty = Content mempty
  mappend = (<>)

run :: RIO SimpleApp ()
run = do
  Content snippets <-
    runConduitRes $
    sourceDirectoryDeep True "content" .|
    filterC (\fp -> takeExtension fp == ".md") .|
    foldMapMC (lift . parseContent)
  let snippetDir = "tmp" </> "snippets"
  createDirectoryIfMissing True snippetDir
  snippetFPs <- runConduit $ yieldMany (Map.toList snippets) .| foldMapMC (\(text, src) -> do
    let digest :: Crypto.Hash.Digest Crypto.Hash.SHA256
        digest = Crypto.Hash.hash $ encodeUtf8 text
        fp = snippetDir </> show digest <.> ".hs"
    exists <- doesFileExist fp
    unless exists $ do
      logDebug $ "Writing new file: " <> fromString fp
      writeFileUtf8 fp text
    pure (Map.singleton fp src)
    )
  runConduitRes $ sourceDirectory snippetDir .| mapM_C (\fp ->
    if
      | takeExtension fp /= ".hs" -> pure ()
      | Just src <- Map.lookup fp snippetFPs -> do
        compiled <- doesFileExist $ replaceExtension fp ".o"
        if compiled
          then logDebug $ "Snippet already compiled: " <> fromString fp
          else do
            logInfo $ "Testing snippet: " <> fromString fp
            proc "ghc" [fp, "-fdefer-typed-holes"] (void . readProcess_)
              `catchAny` \e ->
              error $ "Error with snippet from " ++ show src ++ ": " ++ show e
      | otherwise -> do
        logDebug $ "Removing file: " <> fromString fp
        removeFile fp)

parseContent :: FilePath -> RIO SimpleApp Content
parseContent fp = do
  md <- readFileUtf8 fp
  let html = commonmarkToHtml [] [] md
      Document _ root _ = DOM.parseBSChunks [encodeUtf8 html]
  pure $ goElem root
  where
    goElem (Element "code" _ [NodeContent text])
      | "#!/usr/bin/env stack" `T.isPrefixOf` text =
          Content (Map.singleton (stripWerror text) fp)
    goElem (Element _ _ children) = foldMap goNode children

    goNode (NodeElement e) = goElem e
    goNode _ = mempty

stripWerror :: Text -> Text
stripWerror = T.unlines . filter (not . isWerror) . T.lines
  where
    isWerror = ("-Werror #-}" `T.isSuffixOf`)
