module Main where

import           Conduit                  (awaitForever, runConduitRes, (.|))
import qualified Data.Aeson               as A
import           Data.ByteString.Builder  (byteString)
import qualified Data.Conduit.Combinators as C (sourceFile)
import qualified Network.Wai.Handler.Warp as W
import           Text.Read                (readMaybe)
import qualified UnliftIO.Exception       as E

import           WebbyPrelude

import           Webby.Server

data AppEnv = AppEnv { aeAppName :: Text
                     , aeWEnv    :: WEnv
                     }

instance HasWEnv AppEnv where
    getWEnv = aeWEnv
    setWEnv wEnv appEnv = appEnv { aeWEnv = wEnv }

defaultHandler :: Handler AppEnv
defaultHandler r = text "Oops!"

bracketHandler :: Handler AppEnv
bracketHandler r = E.bracket (liftIO $ print "allocating")
                   (\_ -> liftIO $ print "cleaning up")
                   $ \_ -> do env <- ask
                              text (show $ aeAppName env)

streamingHandler :: Handler AppEnv
streamingHandler r = do
    let streamer w f = runConduitRes $
                       C.sourceFile "/etc/issue"
                       .| awaitForever (\b -> liftIO $ w (byteString b) >> f)

    streamResponse streamer

jsonHandler :: Handler AppEnv
jsonHandler r = json $ A.object [ "language" A..= A.String "Haskell"
                                , "rating" A..= A.String "10"
                                ]
main :: IO ()
main = do
    let routes = [ get "/api/a" (\_ -> text "a")
                 , get "/api/b" (\_ -> text "b")
                 , post "/api/capture/:id" (\_ -> do idM <- getCapture "id"
                                                     text $ show idM
                                           )
                 , get "/aaah" (\_ -> E.throwString "oops!"
                                   )
                 , get "/api/bracket" bracketHandler
                 , get "/api/streaming" streamingHandler
                 , get "/api/json" jsonHandler
                 ]
    myWEnv <- emptyWEnv
    W.runEnv 9000 $ webbyApp (AppEnv "myApp" myWEnv) routes defaultHandler
