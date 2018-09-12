{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
import Data.Hourglass
import Data.EventList.Relative.TimeBody
import Numeric.NonNegative.Class

durCons hours minutes =
  Duration {
    durationHours = hours,
    durationMinutes = minutes,
    durationSeconds = Seconds 0,
    durationNs = NanoSeconds 0
  } 

deriving instance C Duration
  
durSample = durCons (Hours 3) (Minutes 2)

main = do
  let time = durCons (Hours 3) (Minutes 2)
  let events = cons time "e2" $ cons time "e1" empty
  print $ duration events
