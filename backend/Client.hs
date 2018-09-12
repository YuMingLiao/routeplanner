{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Types
import Common

{- Server's api
type API = "getint" :> Get '[JSON] Int
      :<|> "getlist" :> Get '[JSON] Person
-}
api :: Proxy API
api = Proxy

getint :<|> getlist :<|> getdistricts :<|> getlocations = client api

{-
-- | URI scheme to use
data Scheme =
    Http  -- ^ http://
  | Https -- ^ https://

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
data BaseUrl = BaseUrl
  { baseUrlScheme :: Scheme -- ^ URI scheme to use
  , baseUrlHost :: String   -- ^ host (eg "haskell.org")
  , baseUrlPort :: Int      -- ^ port (eg 80)
  , baseUrlPath :: String   -- ^ path (eg "/a/b/c")
  }
-}

queries :: ClientM (Int, [Person])
queries = do
  dest <- getint 
  ppl  <- getlist
  return (dest, ppl)

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (ClientEnv manager' (BaseUrl Http "localhost" 8001 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (dest, ppl) -> do
      print dest
      print ppl

main :: IO ()
main = run
