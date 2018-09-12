{-# Language OverloadedStrings #-}

module Test where

import Google.DistanceMatrix
import Data.Text (Text)
import Data.Vector (Vector)
import System.IO
import MyPrint
import Locations
import Data.List.Split
import Data.Csv
import Data.Matrix
import Types
import Control.Lens
import qualified Data.ByteString.Lazy as BS 

main :: IO ()
main = do
  let topRow = Address <$> (^.address) <$> locations ++ [head locations]
  let fstCol = Address <$> (^.address) <$> locations ++ [head locations]
  print $ length locations
  print $ length $ chunksOf 5 topRow
  super_m <- mapM sequence [[ getDM x y | x <- chunksOf 5 topRow] | y <- chunksOf 5 fstCol] --super matrix
  let dm = foldl1 (<->) $ map (foldl1 (<|>)) $ super_m 
  putStrLn $ show (nrows dm) ++ " " ++ show (ncols dm)
  --BS.writeFile "dm.csv" $ encode $ matrixToVectors $ dm
  writeFile "dm.csv" $ show . toLists $ dm


matrixToVectors :: Matrix a -> [Vector a]
matrixToVectors m = (`getRow` m) <$> [1..(nrows m)]

apikey = "AIzaSyDuTKpi1CbEL3T-Wun8vItk24DYYtm62i4"
getDM x y = either (error "fail") rows <$> distanceMatrix x y Nothing apikey

