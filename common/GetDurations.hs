{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (any)
import Data.Csv hiding (lookup)
import qualified Data.ByteString.Lazy as BS 
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as I
import qualified Data.Vector as V
import Data.Either
import Types
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
import Data.Function
import Debug.Trace
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
import Data.Hourglass as HG
-- Data
import Locations
import WaypointsDM

getDurations :: IO [Seconds]
getDurations = do
  let m_dur_value = elementwise (\x y-> value $ GD.duration x) waypointsDM waypointsDM

  let tupIdn = take (length locations - 1) $ zip [1..] [2..]
  print tupIdn
  myPPrint $ zip tupIdn $ roundUp . Seconds . fromIntegral <$> uncurry (\x y-> getElem x y m_dur_value) <$> tupIdn


roundUp n = case (n `mod` 300) < 150 of  -- 300 secs is 5 minutes
                 True  -> n - (n `mod` 300)
                 False -> n - (n `mod` 300) + 300
 

{-
getDuration :: Location -> Location -> GD.Duration
getDuration la lb = let
    (Just idxA) = join $ (`elemIndex` locations) <$> find (==la) locations
    (Just idxB) = join $ (`elemIndex` locations) <$> find (==lb) locations
    dur = GD.duration $ getElem (idxA+1) (idxB+1) waypointsDM --col row
    in dur

mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f a l = swap <$> runStateT (mapM go l) a
    where
        go i = do
            s <- get
            (s', r) <- lift $ f s i
            put s'
            return r

-}
