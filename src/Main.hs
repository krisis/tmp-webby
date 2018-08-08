module Main where

import qualified Network.Wai.Handler.Warp as W
import qualified UnliftIO.Exception       as E

import           WebbyPrelude

import           Webby.Server

defaultHandler :: Handler
defaultHandler r = text "Oops!"

main :: IO ()
main = do
    let routes = [ get "/api/a" (\_ -> text "a")
                 , get "/api/b" (\_ -> text "b")
                 , post "/api/capture/:id" (\_ -> do idM <- getCapture "id"
                                                     text $ show idM
                                           )
                 , get "/aaah" (\_ -> E.throwString "oops!"
                                   )
                 ]
    W.runEnv 9000 $ webbyApp routes defaultHandler
