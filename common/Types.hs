{-# Language RecordWildCards, FunctionalDependencies, DuplicateRecordFields, TemplateHaskell, FlexibleInstances, DeriveAnyClass, TypeSynonymInstances, MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Types (
   module Types
 ) where
import Prelude hiding (any)
import qualified Data.Types.Isomorphic as Iso
import Data.List hiding (any)
import Data.Csv
import Data.Data
import GHC.Generics
import Data.Maybe
import Data.Text (any,Text,pack,replace)
import Control.Lens hiding ((.=),to,both)
import Control.Arrow ((>>>),(&&&))
import Data.Matrix hiding (toList,trace,(!))
import Data.Tuple.Extra (both)
import Data.Vector (toList,Vector,(!))
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
import Data.Aeson hiding (decode, Result)
import GoogleMapsReflex.JSTypes.LatLng
import GoogleMapsReflex
import GoogleMapsReflex.JSTypes.Marker
import Data.Default
import Data.Ord
import Data.Proxy
import qualified Data.ByteString.Lazy as BS 
import MyPrint
import Google.Directions
import Google.DistanceMatrix as GD (Element(..),Duration(..))
import Data.Hourglass as HG
import WaypointsDM
import qualified Data.List as L

-- For CSV <-> Record
instance FromRecord Text where
  parseRecord v = pack . B.unpack <$> v .! 0

instance ToRecord Text where
  toRecord t = record [
        toField t]

instance FromRecord Int where
  parseRecord v = (read . B.unpack <$> (v .! 0) :: Parser Int)

instance ToRecord Int where
  toRecord i = record [
        toField i]

instance FromRecord Double where
  parseRecord v = (read . B.unpack <$> (v .! 0) :: Parser Double)
instance ToRecord Double where
  toRecord d = record [
        toField d]

instance FromField (Double,Double) where
  parseField s = pure $ (read $ B.unpack s :: (Double,Double))

instance ToField (Double, Double) where
  toField tup = B.pack . show $ tup 

getRecords :: forall a. FromRecord a => Proxy a -> String -> IO [a]  
getRecords _ filename = do
  txt <- BS.readFile filename
  return $ toList $ either error (id) $ (decode NoHeader txt :: Either String (Vector a))  


--Basic DataTypes
data District = District {
   _name :: !Text
 , _code :: !Text
 , _lat  :: !Double
 , _lng  :: !Double
 } deriving (Show, Data, Generic, FromRecord, ToRecord, FromJSON, ToJSON)

data Location = Loc {
   _goal :: !Text
 , _idn  :: !Int
 , _name :: !Text
 , _district :: !Text
 , _address :: !Text
 , _lat :: !Double
 , _lng :: !Double
} deriving (Show, Data, Typeable, Generic, FromRecord, ToRecord, FromJSON, ToJSON)


data Car = Car {
    _idx :: !Int
  , _name :: !Text
  , _origin :: !Int
  , _capacity :: !Int
} deriving (Show, Generic, FromJSON, ToJSON) 

data Person = Person {
    _name :: !Text
 ,  _location :: !Int
 ,  _phoneNumber :: !Text
 } deriving (Show, Data, Typeable, Generic, FromRecord, ToRecord, FromJSON, ToJSON)

data CarsAndPeople = CarsAndPeople {
    _destination :: LocIdx
  , _cars :: [Car]
  , _people :: [Person]
} deriving (Show, Generic, FromJSON, ToJSON)

instance Default CarsAndPeople where
  def = CarsAndPeople 0 [def] [def]

data Result = Result {
    rest :: [Person]
  , carsPlan :: [CarPlan]
} deriving (Show, Generic, FromJSON, ToJSON)

instance Default Result where
  def = Result [def] [def]

type LocIdx = Int
data CarPlan = CarPlan Car [LocIdx] [Person] deriving (Generic, FromJSON, ToJSON)

carPlan :: Car -> [Person] -> CarPlan
carPlan car ps = CarPlan car (L.nub $ L.map _location ps) ps

instance Show CarPlan where
  show (CarPlan (Car idx name ori capacity) ls ps) = 
    "idx=" ++ show idx ++"\n" ++
    "name=" ++ myPShow name ++"\n" ++
    "from=" ++ show ori ++"\n" ++
    "pass=" ++ show ls ++ "\n" ++
    "capacity=" ++ show (length ps) ++ "/" ++ show capacity ++"\n"

instance Default CarPlan where
  def = CarPlan def [0,1] [def]


type Situation = [(Location,[Person])]
data WayPointPlan = WayPointPlan Location [Person]

makeFieldsNoPrefix ''Location
makeFieldsNoPrefix ''Person
makeFieldsNoPrefix ''District
makeFieldsNoPrefix ''Car
makeFieldsNoPrefix ''CarsAndPeople

-- typeclass & instances
-- for comparision & ordering

instance Eq Location where
  a == b = a^.idn == a  ^.idn

instance Eq District where
  a == b = a^.name == b^.name

instance Eq Car where
  a == b = a^.idx == b^.idx

instance Ord Car where
  compare a b = compare (a^.idx) (b^.idx)

instance Eq Person where
  a == b = a^.location == b^.location

instance Ord Person where
  compare a b = compare (a^.location) (b^.location)
         

instance Ord Location where
 compare a b =
   case compareDistrictByName (a^.district) (b^.district) of
        LT -> LT
        GT -> GT
        EQ -> case comparing (^.lat) a b of
                   LT -> GT  --lat lower, distric index higher
                   GT -> LT
                   EQ -> EQ

instance Ord District where
  compare a b = compareDistrictByName (a^.name) (b^.name)

compareDistrictByName :: Text -> Text -> Ordering
compareDistrictByName dname1 dname2 =
  let dnamelist = map (^.name) districts 
      err x = error $ "can't find: " ++ myShow x
      a = maybe (err dname1) id (elemIndex' dname1 dnamelist)
      b = maybe (err dname2) id (elemIndex' dname2 dnamelist)
  in compare a b

-- add word variant
elemIndex' e list = let
  es = produceVariant e
  in listToMaybe . catMaybes $ map (`elemIndex` list) es

produceVariant :: Text -> [Text]
produceVariant e =
  case any (=='台') e of
       True  -> [e, replace "台" "臺" e]
       False -> [e]

--For CustomShow

class Show a => CustomShow a where
  customShow :: a -> String
  customShow = myShow

instance CustomShow Location where
  customShow loc = myShow (loc^.name)

instance CustomShow Person where
  customShow p = myShow (p^.name)

instance CustomShow Car where
  customShow car = show "Car {" ++ "\n"
                ++ myShow (car^.name) ++ ", \n"

instance CustomShow Situation where
  customShow ((l,ps):xs) = customShow l ++ "\n" ++ show (length ps) ++"\n" ++ customShow xs
  customShow [] = ""




-- Default
instance Default Person where
  def = Person "廖予銘" 0 "0912345678" 

instance Default Location where
  def = Loc "休息" 0 "石碇服務區" "新北市" "新北市石碇區文山路一段6號" 25.0077814 121.6323265

defLowerOrigin = Loc "斗南" 17 "全家-斗南大業店" "雲林縣" "雲林縣斗南鎮大業路226號" 23.6949571 120.464005

instance Default Car where
  def = Car 1 "Bus" 1 40

defCars = [
  Car 1 "遊覽車" 17 40,
  Car 2 "交通車" 1 8,
  Car 3 "交通車" 1 8,
  Car 4 "交通車" 17 8,
  Car 5 "徵調車" 0 4
 ]

-- for rending html table

class GSelectors a where
  gselectors :: Proxy a -> [(String, TypeRep)]

instance GSelectors U1 where
  gselectors _ = []

-- Product branch
instance (GSelectors a, GSelectors b) => GSelectors (a :*: b) where
  gselectors _ = gselectors (Proxy :: Proxy a) ++ gselectors (Proxy :: Proxy b)

-- Datatype
instance GSelectors f => GSelectors (M1 D x f) where
  gselectors _ = gselectors (Proxy :: Proxy f)

-- Constructor Metadata
instance GSelectors f => GSelectors (M1 C x f) where
  gselectors _ = gselectors (Proxy :: Proxy f)

-- Selector Metadata && Constructor Parameter
instance (Selector s, Typeable t) => GSelectors (M1 S s (K1 R t)) where
  gselectors _ =
    [ ( selName (undefined :: M1 S s (K1 R t) ()) , typeOf (undefined :: t) ) ]

districts = [
  District {_name = "花蓮縣", _code = "HWA", _lat = 23.7569, _lng = 121.3542},
  District {_name = "宜蘭縣", _code = "ILN", _lat = 24.69295, _lng = 121.7195},
  District {_name = "基隆市", _code = "KLU", _lat = 25.10898, _lng = 121.7081},
  District {_name = "新北市", _code = "NTC", _lat = 24.91571, _lng = 121.6739},
  District {_name = "臺北市", _code = "TPE", _lat = 25.09108, _lng = 121.5598},
  District {_name = "桃園市", _code = "TYU", _lat = 24.93759, _lng = 121.2168},
  District {_name = "新竹縣", _code = "HSH", _lat = 24.70328, _lng = 121.1252},
  District {_name = "新竹市", _code = "HSC", _lat = 24.80395, _lng = 120.9647},
  District {_name = "苗栗縣", _code = "MAL", _lat = 24.48927, _lng = 120.9417},
  District {_name = "臺中市", _code = "TXG", _lat = 24.23321, _lng = 120.9417},
  District {_name = "彰化縣", _code = "CWH", _lat = 23.99297, _lng = 120.4818},
  District {_name = "南投縣", _code = "NTO", _lat = 23.83876, _lng = 120.9876},
  District {_name = "雲林縣", _code = "YUN", _lat = 23.75585, _lng = 120.3897},
  District {_name = "嘉義縣", _code = "CHY", _lat = 23.45889, _lng = 120.574},
  District {_name = "嘉義市", _code = "CYI", _lat = 23.47545, _lng = 120.4473},
  District {_name = "臺南市", _code = "TNN", _lat = 23.1417, _lng = 120.2513},
  District {_name = "高雄市", _code = "KHH", _lat = 23.01087, _lng = 120.666},
  District {_name = "屏東縣", _code = "PCH", _lat = 22.54951, _lng = 120.62},
  District {_name = "臺東縣", _code = "TTT", _lat = 22.98461, _lng = 120.9876}
 ]


getDirections :: Text -> Text -> [Text] -> IO (Either String Directions)
getDirections o d waypoints = do
  r <- directions o d (Just Driving) (Just (Waypoints True waypoints)) False [] Nothing False
  return r


getDurations :: [Seconds]
getDurations = let
  m_dur_value = elementwise (\x y-> value $ GD.duration x) waypointsDM waypointsDM
  tupIdn = take (length locations - 1) $ zip [1..] [2..] --interval from 0 to 28
  in roundUp . Seconds . fromIntegral <$> uncurry (\x y-> getElem x y m_dur_value) <$> tupIdn


roundUp n = case (n `mod` 300) < 150 of  -- 300 secs is 5 minutes
                 True  -> n - (n `mod` 300)
                 False -> n - (n `mod` 300) + 300
 
instance Num HG.Duration where
  d1 + d2 = mappend d1 d2
  (-) = undefined
  (*) = undefined
  negate = undefined
  fromInteger = undefined

instance Default HG.Duration where
  def = Duration (Hours 1) (Minutes 0) (Seconds 0) (NanoSeconds 0)

deriving instance ToJSON Seconds
deriving instance Generic Seconds
--type Estimation = HG.Duration
type Estimation = Seconds

instance Default Seconds where
  def = Seconds 3600

class Show' a where
  show' :: a -> String

locations =
  [ Loc 
      { _goal = "休息"
      , _idn = 0
      , _name = "石碇服務區"
      , _district = "新北市"
      , _address = "新北市石碇區文山路一段6號"
      , _lat = 25.0077814
      , _lng = 121.6323265
      } 
  , Loc 
      { _goal = "台電"
      , _idn = 1
      , _name = "捷運台電大樓站1號出口"
      , _district = "臺北市"
      , _address = "臺北市中正區羅斯福路3段126之5號"
      , _lat = 25.0205742
      , _lng = 121.5279607
      } 
  , Loc 
      { _goal = "行天宮"
      , _idn = 2
      , _name = "7-11權松門市"
      , _district = "台北市"
      , _address = "台北市松江路362號行天宮後方"
      , _lat = 25.0597131
      , _lng = 121.5331637
      } 
  , Loc 
      { _goal = "林口"
      , _idn = 3
      , _name = "A9捷運林口站"
      , _district = "新北市"
      , _address = "新北市林口區八德路290號"
      , _lat = 25.0658916
      , _lng = 121.3620487
      } 
  , Loc 
      { _goal = "南崁"
      , _idn = 4
      , _name = "7-11北崁門市"
      , _district = "桃園市"
      , _address = "桃園市蘆竹區中正路85號"
      , _lat = 25.0430864
      , _lng = 121.2942585
      } 
  , Loc 
      { _goal = "楊梅"
      , _idn = 6
      , _name = "7-11新楊欣門市"
      , _district = "桃園市"
      , _address = "桃園市楊梅區中山北路一段388號"
      , _lat = 24.9089654
      , _lng = 121.1590196
      } 
  , Loc 
      { _goal = "新屋"
      , _idn = 7
      , _name = "全家-平鎮民族店"
      , _district = "桃園市"
      , _address = "桃園市平鎮區民族路二段189號"
      , _lat = 24.9569229
      , _lng = 121.203695
      } 
  , Loc 
      { _goal = "湖口"
      , _idn = 5
      , _name = "7-11豐鈴門市"
      , _district = "新竹縣"
      , _address = "新竹縣湖口鄉安宅四街2號"
      , _lat = 24.8710514
      , _lng = 121.029543
      } 
  , Loc 
      { _goal = "竹北"
      , _idn = 8
      , _name = "7-11縣運門市"
      , _district = "新竹縣"
      , _address = "新竹縣竹北市莊敬南路21號"
      , _lat = 24.8198118
      , _lng = 121.0219328
      } 
  , Loc 
      { _goal = "新竹"
      , _idn = 9
      , _name = "中油光明站"
      , _district = "新竹市"
      , _address = "新竹市光復路二段1號"
      , _lat = 24.7909793
      , _lng = 121.0041462
      } 
  , Loc 
      { _goal = "頭份"
      , _idn = 10
      , _name = "麥當勞-苗栗頭份店"
      , _district = "苗栗縣"
      , _address = "苗栗縣頭份市中華路1335號"
      , _lat = 24.6929207
      , _lng = 120.9143608
      } 
  , Loc 
      { _goal = "休息"
      , _idn = 11
      , _name = "泰安服務區"
      , _district = "台中市"
      , _address = "台中市后里區月眉里九甲七路400號"
      , _lat = 24.3250582
      , _lng = 120.7200078
      } 
  , Loc 
      { _goal = "台中"
      , _idn = 12
      , _name = "全家光明店"
      , _district = "台中市"
      , _address = "台中市台灣大道三段730號黎明路和中港路口"
      , _lat = 24.1711763
      , _lng = 120.6367593
      } 
  , Loc 
      { _goal = "龍井"
      , _idn = 13
      , _name = "全家沙鹿大展店"
      , _district = "台中市"
      , _address = "台中市沙鹿區自強路336號"
      , _lat = 24.0908142
      , _lng = 120.5472429
      } 
  , Loc 
      { _goal = "彰化"
      , _idn = 14
      , _name = "台塑石油加油站"
      , _district = "彰化縣"
      , _address = "彰化縣彰化市中央路292號"
      , _lat = 24.0683342
      , _lng = 120.5263586
      } 
  , Loc 
      { _goal = "員林"
      , _idn = 15
      , _name = "萊爾富-彰湖店"
      , _district = "彰化縣"
      , _address = "彰化縣溪湖鎮員鹿路一段253號"
      , _lat = 23.9533901
      , _lng = 120.5028551
      } 
  , Loc 
      { _goal = "西螺"
      , _idn = 16
      , _name = "萊爾富便利商店雲好店"
      , _district = "雲林縣"
      , _address = "雲林縣莿桐鄉三和137之1號"
      , _lat = 23.7749145
      , _lng = 120.4793991
      } 
  , Loc 
      { _goal = "斗南"
      , _idn = 17
      , _name = "全家-斗南大業店"
      , _district = "雲林縣"
      , _address = "雲林縣斗南鎮大業路226號"
      , _lat = 23.6949571
      , _lng = 120.464005
      } 
  , Loc 
      { _goal = "大林"
      , _idn = 18
      , _name = "統聯"
      , _district = "嘉義縣"
      , _address = "嘉義縣大林鎮明和里305-1號"
      , _lat = 23.6118511
      , _lng = 120.4371367
      } 
  , Loc 
      { _goal = "嘉義"
      , _idn = 19
      , _name = "7-11嘉高門市"
      , _district = "嘉義市"
      , _address = "嘉義市西區北港路1062號"
      , _lat = 23.4924254
      , _lng = 120.3975016
      } 
  , Loc 
      { _goal = "新營"
      , _idn = 20
      , _name = "麥當勞"
      , _district = "台南市"
      , _address = "台南市新營區復興路684號"
      , _lat = 23.3059486
      , _lng = 120.2955164
      } 
  , Loc 
      { _goal = "麻豆"
      , _idn = 21
      , _name = "7-11麻佳門市"
      , _district = "台南市"
      , _address = "台南市麻豆區苓子林17-113號"
      , _lat = 23.1801194
      , _lng = 120.2318626
      } 
  , Loc 
      { _goal = "永康"
      , _idn = 22
      , _name = "萊爾富-永康上永店"
      , _district = "台南市"
      , _address = "台南市永康區中正北路314號"
      , _lat = 23.0415846
      , _lng = 120.2476551
      } 
  , Loc 
      { _goal = "休息"
      , _idn = 23
      , _name = "仁德服務區"
      , _district = "台南市"
      , _address = "台南市仁德區中洲1之16號"
      , _lat = 22.9063136
      , _lng = 120.2503931
      } 
  , Loc 
      { _goal = "路竹"
      , _idn = 24
      , _name = "7-11環球門市"
      , _district = "高雄市"
      , _address = "高雄市路竹區環球路201之27號"
      , _lat = 22.8800211
      , _lng = 120.2720267
      } 
  , Loc 
      { _goal = "楠梓"
      , _idn = 25
      , _name = "統聯客運楠梓站"
      , _district = "高雄市"
      , _address = "高雄市楠梓區興楠路263號交流道下來右手邊"
      , _lat = 22.729777
      , _lng = 120.333537
      } 
  , Loc 
      { _goal = "高雄"
      , _idn = 26
      , _name = "摩斯漢堡高雄中正店"
      , _district = "高雄市"
      , _address = "高雄市苓雅區中正一路122號"
      , _lat = 22.628327
      , _lng = 120.332345
      } 
  , Loc 
      { _goal = "南州"
      , _idn = 27
      , _name = "7-11壽元門市"
      , _district = "屏東縣"
      , _address = "屏東縣南州鄉勝利路5之15號"
      , _lat = 22.4915353
      , _lng = 120.5373739
      } 
  , Loc 
      { _goal = "屏東"
      , _idn = 28
      , _name = "全家長治店"
      , _district = "屏東縣"
      , _address = "屏東縣長治鄉水源路195號"
      , _lat = 22.6967548
      , _lng = 120.557154
      } 
  ] 


