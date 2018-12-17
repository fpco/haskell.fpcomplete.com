-- | All of the simple Markdown routes
module Handler.Markdown where

import Import

getHomeR :: Handler Html
getHomeR = displayMarkdown "home"

getContributeR :: Handler Html
getContributeR = displayMarkdown "contribute"
