{-# Language OverloadedStrings #-}
{-# Language DataKinds #-}
{-# Language TypeApplications #-}
{-# Language RecordWildCards #-}
module Main where
import           Common
import           Data.Proxy
import           Servant.API
import           Servant.Server.Internal.SnapShims
import           Servant.Server hiding (route)
import           Snap
import           Snap.Core
import           Snap.Http.Server
import           Servant.Server.Internal.SnapShims
import           Snap.Util.CORS
import           Snap.Http.Server
import           Snap.Util.FileServe
import           Snap.Util.FileUploads
import           System.Directory
import           System.FilePath ((</>))
import           Control.Monad (void)
import           Control.Monad.IO.Class
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import           Data.Default
import           Types
import           Organize
-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'ExceptT ServantErr IO' monad.
server :: Server API '[BasicAuthCheck (Handler App App) ()] (Handler App App)
server = getEstimationList
--    :<|> list 
--    :<|> listDistricts
--    :<|> listLocations
    :<|> getPlan -- :> ReqBody '[JSON] CarsAndPeople :> Post '[JSON] Plan
    :<|> serveDirectory "static"
  where
    getEstimationList n = return getDurations
    --getPlan :: CarsAndPeople -> Handler Plan
    getPlan CarsAndPeople{..} = do result <- liftIO $ organize _destination _people _cars 
                                   return result
          {-
    list = do
      ppl <- liftIO $ getPeople "../data/people.csv"
      return ppl
    listDistricts = do
      return =<< liftIO $ getDistricts "../data/district.csv"
    listLocations = do
      return =<< liftIO $ getLocations "../data/locations_gps.csv"
-}

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
test :: Handler App App ()
test = serveSnapWithContext api
         (BasicAuthCheck (\_ -> return @(Handler App App) (Authorized ())) :. EmptyContext) server

handleUpload :: MonadSnap m => m ()
handleUpload = void $ handleFileUploads "tmp" defaultUploadPolicy (const $ allowWithMaximumSize $ 10*miB) handler
  where
    miB = 2^(20 :: Int)

    handler pinfo ef = do
      case ef of
        Left e -> putStrLn $ show e ++ show 343
        Right fp -> do
          case partFileName pinfo of
            Nothing -> error "Missing filename"
            Just fname -> do
              renameFile fp $ "upload" </> T.unpack (T.decodeUtf8 fname)


initApp :: SnapletInit App App
initApp = makeSnaplet "myapp" "example" Nothing $ do
  wrapSite $ applyCORS defaultOptions
  addRoutes  [("", test)
             ,("hello", writeText "hello world")
             ,("upload", handleUpload)
             ,("", serveDirectory "static")
             ]
  return App
main :: IO ()
main = serveSnaplet mempty initApp

data App = App

api :: Proxy API 
api = Proxy
