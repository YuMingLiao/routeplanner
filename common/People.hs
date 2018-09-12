module People where
import Types
import Data.Proxy
import MyPrint

getPeople  = do
  ppl <- getRecords (Proxy :: Proxy Person) "../data/people.csv"
  return ppl
  
