{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module DistanceMatrix where

import Data.Matrix
import Google.DistanceMatrix
import Data.List
import Control.Arrow
import Text.Pretty.Simple
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Csv
import qualified Data.Text as T (words,pack)
import Data.Monoid ((<>))
import Data.Text (Text)
import WaypointsDM
import Locations
import qualified Data.Vector as V (cons, toList, fromList, map)
import Data.Vector (Vector)
import Control.Lens
import Types
import qualified Data.ByteString.Lazy as BS 

class Show' a where
  show' :: a -> String

instance Show a => Show' (Matrix a) where
  show' = show . toLists
          
instance Read a => Read (Matrix a) where
  readsPrec _ str = map toMatrix parsedLists
    where
      parsedLists = reads str
      toMatrix (lists, str) = (fromLists lists, str)
              {-
instance ToRecord (Vector Text) where
    toRecord texts = record $ toField <$> V.toList texts
-}
matrixToVectors :: Matrix a -> [Vector a]
matrixToVectors m = (`getRow` m) <$> [1..(nrows m)]

main = do
  let vs = matrixToVectors $ elementwise (const text) waypointsDM waypointsDM 
  let simplified_vs = (V.map simplify) <$> vs  where simplify text = let ws = T.words text in if length ws == 4 then  ws!!0<>" "<>ws!!2 else "0"<>" "<>ws!!0
  let csv = addHeaders (T.pack . show . (^.idn) <$> locations) (T.pack . show . (^.idn) <$> locations) simplified_vs
  BS.writeFile "dm.csv" $ encode csv

addHeaders :: [Text] -> [Text] -> [Vector Text] -> [Vector Text]
addHeaders topRow fstCol vs =
    ("name\name" `V.cons` (V.fromList topRow)) : zipWith (V.cons) fstCol vs

