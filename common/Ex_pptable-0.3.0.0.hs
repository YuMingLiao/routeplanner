{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Text.PrettyPrint.Tabulate
import qualified GHC.Generics as G
import Data.Data

import qualified Text.PrettyPrint.Tabulate as T

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Vector as Vector

data FxCode = USD | EUR | JPY deriving (Show, Data, G.Generic)
instance T.CellValueFormatter FxCode

-- This record type will be nested inside `Stock`
data Price = Price {price::Double, fx_code::FxCode} deriving (Data, G.Generic, Show)

-- if we do not want the `Price` records to be expanded into their own fields
-- then choose `T.DoNotExpandWhenNested`
instance T.Tabulate Price T.ExpandWhenNested
instance T.CellValueFormatter Price

data Stock = Stock {ticker::String, local_price::Price, marketCap::Double} deriving (
    Data, G.Generic, Show)
instance T.Tabulate Stock T.ExpandWhenNested

yahoo =  Stock {ticker="YHOO", local_price=Price 42.29101010 USD, marketCap=40e9}
google = Stock {ticker="GOOG", local_price=Price 774.210101 EUR, marketCap=532.09e9}
amazon = Stock {ticker="AMZN", local_price=Price 799.161717 JPY, marketCap=378.86e9}

tickers = [yahoo, google, amazon]
tickers_vector = Vector.fromList tickers
tickers_map:: Map.Map Integer Stock
tickers_map = Map.fromList [(10, yahoo), (100, google), (1000, amazon)]

printExamples:: IO ()
printExamples = do
    putStrLn "Printing records in a list\n"
    T.printTable tickers

    putStrLn "\nPrinting records in a map with the index.\nNote the `key(s)` are printed as first columns"
    T.printTable tickers_map

    putStrLn "\nPrinting records in a vector\n"
    T.printTable tickers_vector

    -- Sometimes records may have too many fields. In those case, specific fields can
    -- be chosen to be printed. Currently, support for this functionality is
    -- minimal. The 'headers` are not printed. In the future, a function that
    -- can take header labels as a list will be provided.

    putStrLn "\nPrinting specific fields. Note, currently field names are not printed"
    T.printTableWithFlds [T.DFld (price . local_price), T.DFld ticker] tickers_map

    putStrLn "\nPrint nested record in a map, individually"
    T.printTable $ fmap local_price tickers_map

main:: IO ()
main = do
  printExamples

