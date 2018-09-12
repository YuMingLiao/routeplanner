{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Organize where
import Prelude hiding (reverse,span,take,drop,map)
import Data.IxSet
import Data.Data hiding (Proxy)
import Data.Hourglass.Types
import Types
import People
import Data.Ord
import Data.SortedList 
import MyPrint
import Control.Arrow ((***), second)
import Control.Lens ((^.))
import Data.Monoid ((<>))
import qualified Data.List as L
import Data.Traversable (mapAccumL)
import Data.Ord (Down)
import qualified Data.Text as T (unpack)

testDef = do
  ppl <- getPeople
  let destination = 1
  result <- organize destination ppl defCars
  print $ result

organize :: LocIdx -> [Person] -> [Car] -> IO Result -- return the rest of ppl and cars plan
organize destination ppl cars = do
  let sorted_ppl = toSortedList ppl 
  let (fromNorth, fromSouth) = second reverse $ partition ((<=destination)._location) sorted_ppl
  let (northCars, southCars ) = L.partition ((<=destination)._origin) cars  -- need traversable, so list.
  let (North northRest, northCarsPlan) = mapAccumL pickUp' (North fromNorth) northCars 
  let (South southRest, southCarsPlan) = mapAccumL pickUp' (South fromSouth) southCars 
  --putStrLn $ brief northRest northCarsPlan
  --putStrLn $ brief southRest southCarsPlan
  return $ Result (fromSortedList $ northRest <> reverseDown southRest) (northCarsPlan ++ southCarsPlan)


brief :: [Person] -> [CarPlan] -> String
brief sl plans = show (length sl) ++ "people left.\n"
              ++ show plans

data WaitLine a = North (SortedList a) | South (SortedList (Down a)) deriving Show

pickUp' :: WaitLine Person -> Car -> (WaitLine Person, CarPlan)
pickUp' (North sl) car@(Car _ _ origin capacity) = let
  (beforeOrigin, startFromOrigin) = span ((<origin)._location) sl
  (taken, rest) = (take capacity startFromOrigin, drop capacity startFromOrigin)
  in (North $ beforeOrigin <> rest, carPlan car (fromSortedList taken))

pickUp' (South sl) car@(Car _ _ origin capacity) = let
  (beforeOrigin, startFromOrigin) = span ((< Down origin). Down ._location . unDown) sl
  (taken, rest) = (take capacity startFromOrigin, drop capacity startFromOrigin)
  in (South $ beforeOrigin <> rest, carPlan car (unDown <$> fromSortedList taken))

unDown :: Down a -> a
unDown (Down a) = a


          {-
takeByUp :: Car -> SortedList Person -> (SortedList Person, CarPlan) 
takeByUp car@(Car _ _ origin capacity) ls = let 
  (beforeOrigin, startFromOrigin) = span ((/=origin)._location) ls
  (taken, rest) = (take capacity startFromOrigin, drop capacity startFromOrigin)
  in (beforeOrigin <> rest, CarPlan car (fromSortedList taken))

takeByDown :: Car -> SortedList (Down Person) -> (SortedList (Down Person), CarPlan) 
takeByDown car@(Car _ _ origin capacity) ls = let 
  (beforeOrigin, startFromOrigin) = span ((/=origin) . _location . unDown) ls
  (taken, rest) = (take capacity startFromOrigin, drop capacity startFromOrigin)
  in (beforeOrigin <> rest, CarPlan car (unDown <$> fromSortedList taken))


mapAccumL :: (SortedList Person -> Car -> (SortedList Person, CarPlan)) -> SortedList Person -> SortedList Car -> (a, SortedList CarPlan)
mapAccumL f ps cs = runStateL (traverse' (StateL . flip f) cs) ps



instance Eq CarPlan where
  CarPlan carA lsA == CarPlan carB lsB  = carA == carB && lsA == lsB 
instance Ord CarPlan where
  compare (CarPlan carA _) (CarPlan carB _) = compare carA carB 

-}
