{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language TypeOperators #-}
{-# Language DeriveGeneric #-}
{-# Language ScopedTypeVariables #-}

module Common where
import Data.Text as T
import Servant.API
import Data.Proxy
import Reflex.Dom
import GHC.Generics
--import Servant.Reflex 
import Types


type API = "getEstimationList" :> Capture "destination" LocIdx :> Get '[JSON] [Estimation]
--      :<|> "getlist" :> Get '[JSON] [Person]
--      :<|> "getdistricts" :> Get '[JSON] [District]
--      :<|> "getlocations" :> Get '[JSON] [Location]
      :<|> "getPlan" :> ReqBody '[JSON] CarsAndPeople :> Post '[JSON] Result 
      :<|> Raw


