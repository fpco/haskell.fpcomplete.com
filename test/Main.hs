{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Application (getApplicationDev)
import Test.Hspec
import SpiderWeb
import Network.Wai.Handler.Warp (testWithApplication)
import RIO

main :: IO ()
main = hspec $ do
    it "link checking" $
      testWithApplication (snd <$> getApplicationDev) $ \port ->
        void $ runSimpleApp $ download $ fromString $ "http://localhost:" ++ show port
