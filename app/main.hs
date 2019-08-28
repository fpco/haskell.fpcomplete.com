{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Handler.Warp
import Data.Default.Class (def)
import Network.HTTP.Types (status303)

main :: IO ()
main = do
  logware <- mkRequestLogger def
    { outputFormat = Apache FromFallback
    }
  run 3000 $ logware $ \req send ->
    send $ responseBuilder
      status303
      [("Location", "https://tech.fpcomplete.com/haskell" <> rawPathInfo req)]
      "Redirecting"
