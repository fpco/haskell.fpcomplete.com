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
import Data.Yaml
import Network.HTTP.Simple

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
  { _snippets :: !(Map Text MarkdownSource)
  }
  deriving Show
instance Semigroup Content where
  Content a <> Content x = Content (a <> x)
instance Monoid Content where
  mempty = Content mempty
  mappend = (<>)

newtype Resolver = Resolver Text
instance FromJSON Resolver where
  parseJSON = withObject "Resolver" $ \o -> Resolver <$> o .: "resolver"

data MarkdownSource = Local !FilePath | Remote !Text
  deriving Show

newtype MdUrl = MdUrl Text
instance FromJSON MdUrl where
  parseJSON = withObject "MdUrl" $ \o -> MdUrl <$> o .: "url"

getSource :: FilePath -> RIO SimpleApp (Maybe MarkdownSource)
getSource fp
  | takeExtension fp == ".md" = pure $ Just $ Local fp
  | takeExtension fp == ".yaml" = do
      MdUrl url <- decodeFileThrow fp
      pure $ Just $ Remote url
  | otherwise = pure Nothing

run :: RIO SimpleApp ()
run = do
  expectedResolver <- decodeFileThrow "stack.yaml"
  Content snippets <-
    runConduitRes $
    sourceDirectoryDeep True "content" .|
    concatMapMC (lift . getSource) .|
    foldMapMC (lift . parseContent expectedResolver)
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
            proc "stack" ["exec", "--", "ghc", "-fdefer-typed-holes", fp] (void . readProcess_)
              `catchAny` \e ->
              error $ "Error with snippet from " ++ show src ++ ": " ++ show e
      | otherwise -> do
        logDebug $ "Removing file: " <> fromString fp
        removeFile fp)

parseContent
  :: Resolver
  -> MarkdownSource
  -> RIO SimpleApp Content
parseContent (Resolver expectedResolver) mdSrc = do
  md <-
    case mdSrc of
      Local fp -> readFileUtf8 fp
      Remote url -> do
        req <- parseRequestThrow $ T.unpack url
        res <- httpBS req
        case decodeUtf8' $ getResponseBody res of
          Left e -> error $ "Invalid UTF-8 at " ++ show url ++ ": " ++ show e
          Right text -> pure text
  let html = commonmarkToHtml [] [] md
      Document _ root _ = DOM.parseBSChunks [encodeUtf8 html]
  goElem root
  where
    goElem (Element "code" _ [NodeContent text]) =
      case parseHeader text of
        NoHeader -> pure mempty
        InvalidHeader e -> error $ "Invalid Stack header in " ++ show mdSrc ++ ": " ++ e
        ValidHeader -> pure $ Content (Map.singleton (stripWerror text) mdSrc)
    goElem (Element _ _ children) = foldMapM goNode children

    goNode (NodeElement e) = goElem e
    goNode _ = pure mempty

    parseHeader :: Text -> ParseHeader
    parseHeader text =
      case T.lines text of
        "#!/usr/bin/env stack":next:_ ->
          case T.words next of
            "--":"stack":rest
              | "script" `elem` rest -> findResolver rest
              | otherwise -> InvalidHeader "The word 'script' does not appear"
            _ -> InvalidHeader "No -- stack comment following shebang"

        _ -> NoHeader

    findResolver :: [Text] -> ParseHeader
    findResolver [] = InvalidHeader "No resolver found"
    findResolver ("--resolver":x:_)
      | x == expectedResolver = ValidHeader
      | otherwise = InvalidHeader $ "Expected resolver " ++ show expectedResolver ++ ", got " ++ show x
    findResolver (_:xs) = findResolver xs

stripWerror :: Text -> Text
stripWerror = T.unlines . filter (not . isWerror) . T.lines
  where
    isWerror = ("-Werror #-}" `T.isSuffixOf`)

data ParseHeader = NoHeader | InvalidHeader !String | ValidHeader
