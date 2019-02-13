{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Docs
  ( getLearnR
  , getTutorialR
  , getLibraryR
  ) where

import Import
import qualified Data.Map.Strict as Map

getLearnR :: Handler Html
getLearnR = do
  let title = "Learn Haskell"
  docs <- getDocs
  defaultLayout $ do
    setTitle title
    [whamlet|
      <h1>#{title}
      <p>
        Welcome to FP Complete's Haskell education hub!
        This page contains links to content we believe is most helpful
        in learning to create commercial Haskell software.

      <p>
        In addition to free online learning material, we also offer
        training coursing at all levels. For more information, see
        <a href="https://www.fpcomplete.com/training">our training page
        \.

      <div .container>
        <div .row>
          <div .col-lg-4>
            <h2>Basics

            <p>Just getting started with Haskell? We recommend:

            <ul>
              <li>
                <a href=@{GetStartedR}>Follow our Get Started guide
                to get your tooling set up
              <li>
                Read
                <a href="http://haskellbook.com">Haskell Programming from first principles#
                \, co-authored by FP Complete's very own Chris Allen

          <div .col-lg-4>
            <h2>Intermediate

            <p>
              Once you know the basics, it's time to solidify your skills.
              Follow our
              <a href=@{TutorialR "syllabus"}>Haskell syllabus
              for our recommended set of material. You can also review our
              <a href="https://github.com/fpco/applied-haskell">Applied Haskell commercial training#
              \, available freely on Github.

            <p>
              We also recommend checking out our
              <a href=@{TutorialR "best-practices"}>Haskell best practices#
              \.

          <div .col-lg-4>
            <h2>Reference

            <p>
              Finished the syllabus, or looking for specific material?
              Keep reading the rest of this page for a collection of additional reference information. In particular:

            <ul>
              <li>
                <a href="#libraries">Libraries
              <li>
                <a href="#tutorials">Tutorials

      <div .row>
        <div .col-lg-6>
          <h2 #libraries>Libraries
          <p>
            We've put together a
            <a href=@{TutorialR "libraries"}>guide to recommended libraries
            for commercial Haskell development.
            This is a highly opinionated list based on our experiences.
            You can also
            <a href="https://www.stackage.org/lts">browse libraries available on Stackage#
            \.
          <p>The following is a list of all library tutorials provided on this site.
          <ul>
            $forall (name, doc) <- mapToList $ docsLibraries docs
              <li>
                <a href=@{LibraryR name}>#{docTitle doc}

        <div .col-lg-6>
          <h2 #tutorials>Tutorials
          <p>We have the following general tutorials and guides on this site, separate from library-specific documentation.
          <ul>
            $forall (name, doc) <- mapToList $ docsTutorials docs
              <li>
                <a href=@{TutorialR name}>#{docTitle doc}
    |]

getTutorialR :: Text -> Handler Html
getTutorialR = helper docsTutorials

getLibraryR :: Text -> Handler Html
getLibraryR = helper docsLibraries

helper :: (Docs -> Map Text Doc) -> Text -> Handler Html
helper getter name = do
  docs <- getDocs
  doc <- maybe notFound pure
       $ Map.lookup name
       $ getter docs
  defaultLayout $ do
    setTitle $ docTitle doc
    [whamlet|
      <p>
        <a href=@{LearnR}>Return to Learn
        $maybe edit <- docEditLink doc
          |
          <a href=#{edit}>Edit on Github
    |]
    toWidget $ docBody doc
