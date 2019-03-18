{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.GetStarted
  ( getGetStartedR
  , getGetStartedOSR
  ) where

import Import

getGetStartedR :: Handler Html
getGetStartedR = getStarted Nothing

getGetStartedOSR :: OS -> Handler Html
getGetStartedOSR = getStarted . Just

getStarted :: Maybe OS -> Handler Html
getStarted mos = defaultLayout $ do
  setTitle "Get Started with Haskell"
  toWidget
    [lucius|
.os-logos {
  padding: 1em 0;
}

a.os-logo, a.os-logo:hover {
  margin-right: 2em;
  border: 0;
}

.os-faded img {
  opacity: 0.3
}

.os-choose img {
  opacity: 0.8
}
.os-choose:hover img {
  opacity: 1
}

.terminal-sample {
  font-family: "Monaco", "Ubuntu Mono", monospace;
  white-space: pre;
}

.os-logo img {
  width: 5em;
  height: 5em;
}

h2 .counter {
  color: #999;
}

pre {
  max-width: 30em;
}
    |]
  [whamlet|
    <h1>Get Started
    <p>Quick steps to get up and running with Haskell.
    <div .container>
      <div .row>
        <div .col-md-12>
          <h2>
            <span .counter>1 #
            Download Haskell Stack
      <div .row>
        <div .col-md-6>
          ^{operatingSystems mos}
          ^{operatingSystemDownload mos}
        <div .col-md-6>
          ^{downloadContents}
    <div .container>
      <h2>
        <span .counter>2 #
        Running Haskell programs
      <div .row>
        <div .col-md-6>^{runScripts}
        <div .col-md-6>^{writePackage}
    <div .container>
      <div .row>
        <div .col-md-12>^{nextSteps}
    <div .container>
      <div .row>
        <div .col-md-12>^{furtherInformation}
  |]

operatingSystems :: Maybe OS -> Widget
operatingSystems mos = do
  case mos of
    Nothing -> [whamlet|<p>Choose your operating system:|]
    Just os ->
      [whamlet|
        <p>
          Chosen operating system: #
          <b>#{os}
      |]
  [whamlet|
    <p .os-logos>
      $forall os <- enumFromTo minBound maxBound
        <a .os-logo href=@{GetStartedOSR os} title=#{os} .#{osChoice os}>
          <img src=@{StaticR (osLogo os)}>
  |]
  where
    osChoice :: OS -> Text
    osChoice os =
      case mos of
        Nothing -> "os-choose"
        Just os'
          | os == os' -> "os-selected"
          | otherwise -> "os-faded"

osLogo :: OS -> Route Static
osLogo OSX = img_apple_logo_svg
osLogo Windows = img_windows_logo_svg
osLogo Linux = img_linux_logo_svg

osxWindow :: Text -> Widget -> Widget
osxWindow title content =
  [whamlet|
    <div .osx-window>
      <div .window>
        <div .titlebar>
          <div .buttons>
            <div .closebtn>
              <a .closebutton href=#>
                <span>
                  <strong>x
            <div .minimize>
              <a .minimizebutton href=#>
                <span>
                  <strong>&ndash;
            <div .zoom>
              <a .zoombutton href=#>
                <span>
                  <strong>+
          <span .title-bar-text>#{title}
        <div .content>^{content}
  |]

operatingSystemDownload :: Maybe OS -> Widget
operatingSystemDownload Nothing = pure ()
operatingSystemDownload (Just OSX) = do
  [whamlet|<p>Run the following in your terminal:|]
  osxWindow "Terminal"
    [whamlet|
      <div .terminal-sample>
        <span .noselect>$ #
        <span>curl -sSL https://get.haskellstack.org/ | sh
    |]
  [whamlet|
    <p>
      <a href="http://docs.haskellstack.org/en/stable/install_and_upgrade/#mac-os-x">
        More detailed installation information
  |]
operatingSystemDownload (Just Windows) =
  [whamlet|
    <p>Download and run the installer:
    <p>
      <a .btn .btn-primary href="https://www.stackage.org/stack/windows-x86_64-installer">
        Windows 64-bit installer
    <p .muted-choices>
      For those on older machines, or running a legacy OS, we also offer a 32-bit version: #
      <a href="https://www.stackage.org/stack/windows-i386-installer">
        Windows 32-bit
  |]
operatingSystemDownload (Just Linux) = do
  [whamlet|<p>Run the following in your terminal:|]
  osxWindow "Terminal"
    [whamlet|
      <div .terminal-sample>
        <span .noselect>$ #
        <span>wget -qO- https://get.haskellstack.org/ | sh
    |]
  [whamlet|
    <p>
      <a href="http://docs.haskellstack.org/en/stable/install_and_upgrade/#linux">
        More detailed installation information
  |]

downloadContents :: Widget
downloadContents =
  [whamlet|
    <p>With the Haskell Stack you get a comprehensive development environment for Haskell:
    <ul>
      <li>
        <b>Stack
        : A project builder for multi-package Haskell projects.
      <li>
        <b>GHC
        : A compiler and interpreter for Haskell programs.
      <li>
        <b>Haddock
        : A documentation generator for Haskell packages.
      <li>
        <b>Stackage
        : A curated repository of thousands of packages installed on demand.
  |]

runScripts :: Widget
runScripts =
  [whamlet|
    <p>To quickly run a Haskell script:
    <ol>
      <li>
        <p>
          Copy the following content into a file called #
          <code>HelloWorld.hs
        <pre>
          <code .haskell>
            #!/usr/bin/env stack
            -- stack --resolver lts-13.7 script

            main :: IO ()
            main = putStrLn "Hello World"
      <li>Open up a terminal and run <code>stack HelloWorld.hs</code>.
    <p>Done!
  |]

writePackage :: Widget
writePackage = do
  [whamlet|<p>Start on a proper Haskell package. Run the following in your terminal:|]
  osxWindow "Terminal"
    [whamlet|
      <div .terminal-sample>
        $forall l <- ls
          <span .noselect>$ #
          <span>#{l}
    |]
  where
    ls =
      [ "stack new new-project rio" :: Html
      , "cd new-project"
      , "stack run"
      ]

nextSteps :: Widget
nextSteps =
  [whamlet|
    <h2>
      <span .counter>3 #
      Next steps
    <p>
      Congratulations, you're setup to start writing #
      Haskell code! We've broken down next steps into a few #
      common workflows with Stack. If you're not sure where #
      to start, we recommend reading them in order.
    <ul>
      $forall (title, name) <- nextLinks
        <li>
          <a href=@{TutorialR name}>#{title}
  |]
    where
      nextLinks :: [(Html, Text)]
      nextLinks =
        [ ("How to Play", "stack-play")
        , ("How to Script", "stack-script")
        , ("How to Build", "stack-build")
        ]

furtherInformation :: Widget
furtherInformation =
  [whamlet|
    <h2>
      <span .counter>4 #
      Further Information
    <p>
      Congratulations, you're setup to start writing #
      Haskell code! Now you're ready to:
    <ul>
      $forall (title, route) <- nextLinks
        <li>
          <a href=@{route}>#{title}
  |]
  where
    nextLinks :: [(Html, Route App)]
    nextLinks =
      [ ("Learn about Haskell the language", LearnR)
      , ("Contribute to a Haskell project", ContributeR)
      , ("Promote Haskell at your workplace", PromoteR)
      , ("Interact with the Haskell community", CommunityR)
      , ("Learn about FP Complete's Haskell Success program", SuccessR)
      ]
