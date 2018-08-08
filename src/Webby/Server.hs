{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module Webby.Server
  ( WebbyM
  , Handler
  , RoutePattern
  , Routes

  -- | Routing and handlers
  , mkRoute
  , post
  , get
  , put

  -- | Captures
  , captures
  , getCapture

  -- | Response modification
  , setStatus
  , addHeader
  , setHeader
  , json
  , text
  , streamResponse

  -- | Top Level
  , webbyApp

  -- Application context
  , HasWEnv(..)
  , WEnv
  , emptyWEnv
  ) where


import           Control.Monad.Reader (withReaderT)
import qualified Data.Aeson           as A
import qualified Data.Binary.Builder  as Bu
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as T
import qualified UnliftIO             as U
import qualified UnliftIO.Concurrent  as Conc
import qualified UnliftIO.Exception   as E

import           WebbyPrelude

data WEnv = WEnv { weResp     :: Conc.MVar WyResp
                 , weCaptures :: [(Text, Text)]
                 }
emptyWEnv :: IO WEnv
emptyWEnv = do rspVar <- Conc.newEmptyMVar
               return $ WEnv rspVar []

class HasWEnv a where
    getWEnv :: a -> WEnv
    setWEnv :: WEnv -> a -> a

data WyResp = WyResp { wrStatus    :: Status
                     , wrHeaders   :: ResponseHeaders
                     , wrRespData  :: Either StreamingBody Bu.Builder
                     , wrResponded :: Bool
                     }

defaultWyResp :: WyResp
defaultWyResp = WyResp status200 [] (Right Bu.empty) False

newtype WebbyM env a = WebbyM
    { unWebbyM :: ReaderT env (ResourceT IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader env)

instance U.MonadUnliftIO (WebbyM env) where
    askUnliftIO = WebbyM $ ReaderT $
                  \env -> U.withUnliftIO $
                  \u -> return $
                  U.UnliftIO (U.unliftIO u . flip runReaderT env . unWebbyM)


runWebbyM :: HasWEnv env => env -> WebbyM env a -> IO a
runWebbyM env = runResourceT . flip runReaderT env . unWebbyM

-- To be called before request processing begins. Set up default
-- status for the response in the vault
newWEnv :: [(Text, Text)] -> IO WEnv
newWEnv captures = do wVar <- Conc.newMVar defaultWyResp
                      return $ WEnv wVar captures

captures :: HasWEnv env => WebbyM env [(Text, Text)]
captures = asks (weCaptures . getWEnv)

getCapture :: HasWEnv env => Text -> WebbyM env (Maybe Text)
getCapture t = do cs <- captures
                  return $ fmap snd $ headMay $ filter ((== t) . fst) cs

setStatus :: HasWEnv env => Status -> WebbyM env ()
setStatus st = do
    wVar <- asks (weResp . getWEnv)
    Conc.modifyMVar_ wVar $ \wr -> return $ wr { wrStatus = st}

addHeader :: HasWEnv env => Header -> WebbyM env ()
addHeader h = do
    wVar <- asks (weResp . getWEnv)
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                  return $ wr { wrHeaders = hs ++ [h] }

-- similar to addHeader but replaces a header
setHeader :: HasWEnv env => Header -> WebbyM env ()
setHeader (k, v) = do
    wVar <- asks (weResp . getWEnv)
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                      ohs = filter ((/= k) . fst) hs
                  return $ wr { wrHeaders = ohs ++ [(k, v)] }

text :: HasWEnv env => Text -> WebbyM env ()
text t = do
    setHeader (hContentType, "text/plain; charset=utf-8")
    wVar <- asks (weResp . getWEnv)
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromByteString $
                                          encodeUtf8 t }

json :: (HasWEnv env, A.ToJSON a) => a -> WebbyM env ()
json j = do
    setHeader (hContentType, "application/json; charset=utf-8")
    wVar <- asks (weResp . getWEnv)
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromLazyByteString $
                                          A.encode j }


streamResponse :: HasWEnv env => StreamingBody -> WebbyM env ()
streamResponse s = do
    wVar <- asks (weResp . getWEnv)
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Left s }

data PathSegment = Literal Text
                 | Capture Text
                 deriving (Eq, Show)

-- A route pattern specifies a HTTP method and a list of path segments
-- that must match.
data RoutePattern = RoutePattern Method [PathSegment]
                  deriving (Eq, Show)

type Handler env = Request -> WebbyM env ()

type Routes env = [(RoutePattern, Handler env)]

type Captures = [(Text, Text)]

doesMatch :: [PathSegment] -> [Text] -> Maybe Captures
doesMatch [] [] = Just []
doesMatch [] _ = Nothing
doesMatch _ [] = Nothing
doesMatch (Literal t:xs) (y:ys) | t == y = doesMatch xs ys
                                | otherwise = Nothing
doesMatch (Capture t:xs) (y:ys) = let c = (t, y)
                                  in fmap (c:) $ doesMatch xs ys

matchPattern :: Request -> RoutePattern -> Maybe Captures
matchPattern r (RoutePattern mthd ps)
    | requestMethod r == mthd = doesMatch ps (pathInfo r)
    | otherwise = Nothing

matchRequest :: Request -> Routes env -> Maybe (Captures, Handler env)
matchRequest req routes = do
    let rM = headMay $ filter (\(_, _, k) -> isJust k) $
             map (\(pat, h) -> (pat, h, matchPattern req pat)) routes
    (_, h, csM) <- rM
    cs <- csM
    return (cs, h)

webbyApp :: HasWEnv env => env -> Routes env -> Handler env -> Application
webbyApp appEnv routes defaultHandler req respond = do
    let (cs, handler) = fromMaybe ([], defaultHandler) $ matchRequest req routes
    wEnv <- newWEnv cs
    let appEnv' = setWEnv wEnv appEnv
    E.handleAny (\e -> respond $ responseLBS status500 []
                        "Something went wrong") $ do
        runWebbyM appEnv' (handler req)
        let wVar = weResp wEnv
        wr <- Conc.takeMVar wVar
        case wrRespData wr of
          Left s  -> respond $ responseStream (wrStatus wr) (wrHeaders wr) s
          Right b -> do
              let clen = LB.length $ Bu.toLazyByteString b
              respond $ responseBuilder (wrStatus wr)
                  (wrHeaders wr ++ [(hContentLength, show clen)]) b

text2PathSegments :: Text -> [PathSegment]
text2PathSegments path =
    let mayCapture t = if ":" `T.isPrefixOf` t
                       then Just $ T.drop 1 $ t
                       else Nothing

        mkSegs [] = []
        mkSegs (p:ps) = maybe (Literal p) Capture
                        (mayCapture p) : mkSegs ps

        fixPath = bool identity (drop 1) (T.isPrefixOf "/" path)

    in mkSegs $ fixPath $ T.splitOn "/" path

mkRoute :: Method -> Text -> Handler env -> (RoutePattern, Handler env)
mkRoute m p h = (RoutePattern m (text2PathSegments p), h)

post :: Text -> Handler env -> (RoutePattern, Handler env)
post = mkRoute methodPost

get :: Text -> Handler env -> (RoutePattern, Handler env)
get = mkRoute methodGet

put :: Text -> Handler env -> (RoutePattern, Handler env)
put = mkRoute methodPut
