{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , text
  , streamResponse

  -- | Top Level
  , webbyApp
  ) where


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

data WyResp = WyResp { wrStatus    :: Status
                     , wrHeaders   :: ResponseHeaders
                     , wrRespData  :: Either StreamingBody Bu.Builder
                     , wrResponded :: Bool
                     }

defaultWyResp :: WyResp
defaultWyResp = WyResp status200 [] (Right Bu.empty) False

newtype WebbyM a = WebbyM
    { unWebbyM :: ReaderT WEnv (ResourceT IO) a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader WEnv)

instance U.MonadUnliftIO WebbyM where
    askUnliftIO = WebbyM $ ReaderT $
                  \r -> U.withUnliftIO $
                  \u -> return $
                        U.UnliftIO (U.unliftIO u . flip runReaderT r . unWebbyM)

runWebbyM :: WEnv -> WebbyM a -> IO a
runWebbyM env = runResourceT . flip runReaderT env . unWebbyM

-- To be called before request processing begins. Set up default
-- status for the response in the vault
newWEnv :: [(Text, Text)] -> IO WEnv
newWEnv captures = do wVar <- Conc.newMVar defaultWyResp
                      return $ WEnv wVar captures

captures :: WebbyM [(Text, Text)]
captures = asks weCaptures

getCapture :: Text -> WebbyM (Maybe Text)
getCapture t = do cs <- captures
                  return $ fmap snd $ headMay $ filter ((== t) . fst) cs

setStatus :: Status -> WebbyM ()
setStatus st = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $ \wr -> return $ wr { wrStatus = st}

addHeader :: Header -> WebbyM ()
addHeader h = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                  return $ wr { wrHeaders = hs ++ [h] }

-- similar to addHeader but replaces a header
setHeader :: Header -> WebbyM ()
setHeader (k, v) = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> do let hs = wrHeaders wr
                      ohs = filter ((/= k) . fst) hs
                  return $ wr { wrHeaders = ohs ++ [(k, v)] }

text :: Text -> WebbyM ()
text t = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Right $ Bu.fromByteString $
                                          encodeUtf8 t }

streamResponse :: StreamingBody -> WebbyM ()
streamResponse s = do
    wVar <- asks weResp
    Conc.modifyMVar_ wVar $
        \wr -> return $ wr { wrRespData = Left s }

data PathSegment = Literal Text
                 | Capture Text
                 deriving (Eq, Show)

-- A route pattern specifies a HTTP method and a list of path segments
-- that must match.
data RoutePattern = RoutePattern Method [PathSegment]
                  deriving (Eq, Show)

type Handler = Request -> WebbyM ()

type Routes = [(RoutePattern, Handler)]

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

matchRequest :: Request -> Routes -> Maybe (Captures, Handler)
matchRequest req routes = do
    let rM = headMay $ filter (\(_, _, k) -> isJust k) $
             map (\(pat, h) -> (pat, h, matchPattern req pat)) routes
    (_, h, csM) <- rM
    cs <- csM
    return (cs, h)

webbyApp :: Routes -> Handler -> Application
webbyApp routes defaultHandler req respond = do
    let (cs, handler) = fromMaybe ([], defaultHandler) $ matchRequest req routes
    wEnv <- newWEnv cs

    E.handleAny (\e -> respond $ responseLBS status500 []
                        "Something went wrong") $ do
        runWebbyM wEnv (handler req)
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

mkRoute :: Method -> Text -> Handler -> (RoutePattern, Handler)
mkRoute m p h = (RoutePattern m (text2PathSegments p), h)

post :: Text -> Handler -> (RoutePattern, Handler)
post = mkRoute methodPost

get :: Text -> Handler -> (RoutePattern, Handler)
get = mkRoute methodGet

put :: Text -> Handler -> (RoutePattern, Handler)
put = mkRoute methodPut
