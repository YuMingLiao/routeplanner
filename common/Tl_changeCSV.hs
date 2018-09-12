{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CSVParser where
import Prelude hiding (take)
import Data.Csv
import qualified Data.ByteString.Lazy as BS 
import Data.Text hiding (map, length, zipWith, groupBy, intersperse)
import qualified Data.Vector as V
import Data.Either
import Types
import Control.Lens
import Data.Function
import Data.List hiding (take)
import MyPrint

getDistricts :: String -> IO ()
getDistricts filename = do
  txt <- BS.readFile filename
  let dList = V.toList $ either error (id) $ (decode NoHeader txt :: Either String (V.Vector District)) 
  mapM_ putStr $ intersperse ",\n" $ map myShow dList      



  --BS.writeFile "groupBy.csv" $ encode $ map toLoc2 lList'

switch d =
  let x = d^.lat
      y = d^.lng
  in set lng x (set lat y d)

main = do
  getDistricts "../data/IslandAsLine.csv"
