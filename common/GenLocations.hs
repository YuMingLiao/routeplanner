import Types
import Data.Proxy
import MyPrint
main = do
  locs <- getRecords (Proxy :: Proxy Location) "../data/locs_gps_district.csv"
  writeFile "Locations.hs" $ myPShowNoColor locs
  
