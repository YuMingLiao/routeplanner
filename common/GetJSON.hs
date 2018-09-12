import Types
import Data.Aeson hiding (Result)
import Data.Default
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSL8
import People

main = do
        ppl <- getPeople
        BSL8.writeFile "test.json" $ encode $ CarsAndPeople 1 defCars ppl

testDef = (def :: CarsAndPeople)  
