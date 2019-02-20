{-# LANGUAGE OverloadedStrings #-}
-- | All of the simple Markdown routes
module Handler.Markdown where

import Import

displayMarkdown :: Text -> Handler Html
displayMarkdown = docHelper docsPages

getHomeR :: Handler Html
getHomeR = displayMarkdown "home"

getContributeR :: Handler Html
getContributeR = displayMarkdown "contribute"

getPromoteR :: Handler Html
getPromoteR = displayMarkdown "promote"

getSuccessR :: Handler Html
getSuccessR = displayMarkdown "success"

getCommunityR :: Handler Html
getCommunityR = displayMarkdown "community"

getAboutR :: Handler Html
getAboutR = displayMarkdown "about"

getPhilosophyR :: Handler Html
getPhilosophyR = displayMarkdown "philosophy"

getSyllabusR :: Handler Html
getSyllabusR = displayMarkdown "syllabus"
