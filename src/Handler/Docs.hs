{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Docs
  ( getLearnR
  , getTutorialR
  , getLibraryR
  ) where

import Import

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
        <a href="https://www.fpcomplete.com/training">our training page#
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
              <a href=@{SyllabusR}>Applied Haskell syllabus
              for our recommended set of material. If you'd like a more hands-on training, check out our
              <a href=@{SuccessR}>Haskell Success program#
              \.

            <p>
              We also recommend checking out our
              <a href=@{TutorialR "best-practices"}>Haskell best practices#
              \.

            <p>
              When you're ready, it's a great idea to try
              <a href=@{ContributeR}>contributing to an open source Haskell project#
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
      <dl>
        $forall (name, page) <- mapToList $ docsLibraries docs
          <dt>
            <a href=@{LibraryR name}>
              <b>#{name}
          <dd>#{pageTitle page}

      <h2 #tutorials>Tutorials
      <p>We have the following general tutorials and guides on this site, separate from library-specific documentation.
      <ul>
        $forall (name, page) <- mapToList $ docsTutorials docs
          <li>
            <a href=@{TutorialR name}>#{pageTitle page}
    |]

getTutorialR :: Text -> Handler Html
getTutorialR = docHelper docsTutorials (\_ _ -> mempty)

getLibraryR :: Text -> Handler Html
getLibraryR = docHelper docsLibraries $ \name _ ->
  [shamlet|
    <p .lead>
      The
      <a href="https://www.stackage.org/package/#{name}">#{name}
      library
  |]
