{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CSVParser where
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
import Google.Directions
import Data.Aeson.Text (encodeToLazyText) 
import Data.Aeson.Encode.Pretty
import qualified Text.PrettyPrint.Tabulate as T
import Data.Either.Extra

-- Data
import Locations
import WaypointsDM

main :: IO ()
main = do
  islandAsLine <- getRecords (Proxy :: Proxy District) "../data/IslandAsLine.csv"
  people <- getRecords (Proxy :: Proxy Person) "../data/people.csv"
  
  let locsMap = M.fromList $ map ((^.idn) &&& id) locations 

  let (upper,lower) = split' destination locations

  let fstFromSouth = head lower 
  let sortedPpl = sortBy (comparing (^.location)) people
  let groupedPpl = groupBy' (^.location) sortedPpl
  let groupedPpl' = ((fromJust . (`M.lookup` locsMap). fst) &&& snd) <$> groupedPpl
  let (upper_ppl,lower_ppl) = (id *** reverse) $ partition (ifHigherThan fstFromSouth) groupedPpl'
  let upper_total = sum $ map length $ snd $ unzip upper_ppl
  let lower_total = sum $ map length $ snd $ unzip lower_ppl
  let upper_resources = [def { _name="Bus1", _capacity=40 }
                        ,def { _name="Small1", _capacity=8 }]
--                        ,def { _name="Small2", _capacity=8 } :: Car]
  let lower_resources = [def { _name="Bus1"  , _origin = defLowerOrigin, _capacity=40 }
                        ,def { _name="Small1", _origin = defLowerOrigin, _capacity=8 }]

  myPrint lower_resources
  putStrLn $ customShow $ lower_ppl
  let loRes = mapAccumL (pickup FromSouth) lower_ppl lower_resources
  putStrLn $ customShow $ fst loRes
  mapM_ myPrint $ map customShow $ snd loRes

  let o = (head (snd loRes))^.origin^.address
  let waypoints = map (^.address) (map fst ((head (snd loRes))^.plan))
  let d =  destination^.address
  Right res <- getDirections o d waypoints
  myPrint res
  T.ppTable $ routes res
  I.writeFile "directions.json" (encodeToLazyText res)
  BS.writeFile "directions2.json" (encodePretty res)
  return ()

mapAccumM :: (Monad m, Traversable t) => (a -> b -> m (a, c)) -> a -> t b -> m (a, t c)
mapAccumM f a l = swap <$> runStateT (mapM go l) a
    where
        go i = do
            s <- get
            (s', r) <- lift $ f s i
            put s'
            return r


countBusNeeded :: Int -> Int
countBusNeeded n = let (divided,remain) = ((`div` 40)&&&(`mod` 40)) n 
                       overhalf = remain `div` 20
                   in divided + overhalf

 
ifHigherThan :: Location -> (Location,a) -> Bool
ifHigherThan la (lb,_) = do
  case compare la lb of
    GT -> True
    EQ -> False
    LT -> False

printTakenResult :: (Int, [Person], [Location], Situation) -> IO ()
printTakenResult (taken, takenList, waypoints, remain) = do
  myPrint $ taken 
  myPrint $ map (^.name) takenList
  myPrint $ map (^.name) waypoints 
  myPrint $ ((^.name) *** length) <$> remain

data Direction = FromNorth | FromSouth

pickup :: Direction -> Situation -> Car -> (Situation, Car)
pickup from xss car@Car{..} = 
  let (taken,plan,remain) = takeFrom _capacity from _origin xss
      car' = car{ _plan = plan }
  in (remain, car')

takeFrom :: Int -> Direction -> Location -> Situation -> (Int, Situation, Situation)
takeFrom 0 from _ xss = (0,[],xss)
takeFrom n from start ((l,ps):xs) = do 
  let ord = case from of
                 FromNorth -> compare start l 
                 FromSouth -> comparing Down start l
  case ord of
       LT -> take' n ((l,ps):xs)
       EQ -> take' n ((l,ps):xs)
       GT -> let (taken,plan,remain) = takeFrom n from start xs
             in (taken,plan, (l,ps):remain)

--dropBehind :: Direction -> Car -> Situation -> IO (

reverseOrd :: Ordering -> Ordering
reverseOrd ord = 
  case ord of
       GT -> LT
       EQ -> EQ
       LT -> GT

take' :: Int -> Situation -> (Int, Situation, Situation)
take' 0 pss = (0, [], pss) --no more seats
take' n []  = (0, [], [])  --no one need pickup  
take' n ((l,ps):pss) =
  let (n'', taken'', remain'' ) = take'' n ps
      (n', plan', pss') = take' (n-n'') pss
  in (n'' + n',(if n'' /=0 then ((l,taken''):) else id) plan', (l,remain''):pss')

take'' :: Int -> [Person] -> (Int, [Person], [Person])
take'' n xs =
  let taken  = take n xs
      remain = drop n xs
  in (length taken, taken, remain)

groupBy' :: Eq b => (a->b) -> [a] -> [(b,[a])]
groupBy' f xs = ((f . head) &&& id) <$> groupBy ((==) `on` f) xs

splitOn' :: Int -> [(Int, a)] -> ([(Int, a)], [(Int, a)])
splitOn' n ((a,b) : xs) =
  case compare n a of
    GT -> let (c,d) = splitOn' n xs in (c++[(a,b)],d)
    EQ -> ([],xs)
    LT -> error "impossible to get here"


splitPeople :: Int -> [(Int,Person)] -> ([(Int,Person)],[(Int,Person)])
splitPeople n ((a,b):xs) = 
  case compare n a of
    GT -> let (c,d) = splitPeople n xs in (c++[(a,b)],d)
    EQ -> ([],xs)
    LT -> error "impossible to get here"

-- a -> [a] -> IO ([a], [a])
-- a -> [(a,b)] -> IO ([(a,b)],[a,b])

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM p l = sel l ([],[])
  where
    sel [] a = return a
    sel (x:xs) (ts,fs) = do
        r <- p x
        sel xs $ if r then (x:ts,fs) else (ts,x:fs)


split' :: Location -> [Location] -> ([Location],[Location])
split' dest (x:xs) = do
  case compare dest x of
    GT -> let (a,b) = split' dest xs
          in (x:a,b)
    EQ -> error "There are supposed not to be equal locations."
    LT -> let (a,b) = split' dest xs
          in (a,x:b)

split' dest [] = ([],[]) 

mkList :: [Location] -> M.IntMap [Person] -> [[Person]]
mkList ls m =  catMaybes $ (`M.lookup` m) . (^.idn) <$> ls 

zip' ((a,b):xs) ((c,d):ys) = 
  case compare a c of
       GT -> zip' ((a,b):xs) ys
       EQ -> (a,b,d) : zip' xs ys
       LT -> zip' xs ((c,d):ys)
zip' [] _ = []
zip' _ [] = []

--helper functions
--                   title
inspect :: Show a => T.Text -> a -> IO ()
inspect title a = do
  putStrLn . myShow $ title
  putStrLn . myShow $  a
  putStrLn ""


-- Default & Test

destination = Loc {
     _goal = "destination"
   , _idn = 100
   , _name = "myhome"
   , _district = "南投縣"
   , _address = "南投縣埔里鎮中山路四段219號"
   , _lat = 23.971252 
   , _lng = 120.927988
  } 

destination2 = Loc {
     _goal = "destination"
   , _idn = 100
   , _name = "myhome"
   , _district = "花蓮縣"
   , _address = "花蓮縣花蓮市府前路17號"
   , _lat = 25.0558581
   , _lng = 121.59189099999999
  } 


