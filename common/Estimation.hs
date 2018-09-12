{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Estimation where
import Prelude hiding (any)
import Data.Hourglass
import Data.EventList.Absolute.TimeBody
import Data.Default
import Data.Hourglass as HG
import Data.Aeson
import GHC.Generics
import Data.Default
import Data.Csv hiding (lookup)
import qualified Data.ByteString.Lazy as BS 
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as I
import qualified Data.Vector as V
import Data.Either
import Control.Lens hiding (both)
import Data.Function
import Data.List hiding (take, any)
import Data.Eq
import Data.Ord
import Data.Proxy
import Control.Monad
import Control.Monad.Loops
import MyPrint
import Data.Maybe
import Data.Tuple.Extra
import qualified Data.IntMap.Strict as M
import Data.Maybe
import Control.Monad.State.Lazy
import Data.Default
import Text.Pretty.Simple
import Data.Aeson.Text (encodeToLazyText) 
import Data.Aeson.Encode.Pretty
import qualified Text.PrettyPrint.Tabulate as T
import Data.Either.Extra
import Data.Matrix
import Google.DistanceMatrix as GD (Element(..),Duration(..))
-- Data
import Locations
import WaypointsDM

getDurations :: IO [Seconds]
getDurations = do
  let m_dur_value = elementwise (\x y-> value $ GD.duration x) waypointsDM waypointsDM

  let tupIdn = take (length locations - 1) $ zip [1..] [2..]



instance Num Duration where
  d1 + d2 = mappend d1 d2
  (-) = undefined
  (*) = undefined
  negate = undefined
  fromInteger = undefined

instance Default Duration where
  def = Duration (Hours 1) (Minutes 0) (Seconds 0) (NanoSeconds 0)

instance Default (T Duration Text) where
  def = cons def "event1" $ cons def "event2" $ cons def "event3" empty

deriving instance ToJSON Seconds
deriving instance Generic Seconds
--type Estimation = HG.Duration
type Estimation = Seconds

instance Default Seconds where
  def = Seconds 3600

class Show' a where
  show' :: a -> String



        {-
instance  Show' Estimation where
  show' Duration{..} = show durationHours ++ " " ++ show durationMinutes 
 -}
