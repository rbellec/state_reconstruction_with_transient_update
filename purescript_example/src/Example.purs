module Example where

import Events (ExperimentalEvents, Modification(..))
import Data.Map as Map
import Data.Tuple (Tuple(..))

-- | Tests datas. Will be moved in tests.

-- | Scenario :
--  Event date is written date first and + or - if its a new event or the end of a temp event.
-- 
-- 1+              2+      3+                          4-
-- |               |       |---------------------------|
-- |---------------|-------|---------------------------|--------------------------------------------------------------------------
-- creation        |    Temp modif                 end modif 
-- hours: 35       |    low_season_worktime     hours back to 20
--                 |    hours -> 10        
--                 |
--             to_half_time 
--             hours -> 20

-- initial event
contract_creation :: ExperimentalEvents
contract_creation = PermanentModificiation{
    effectDate : 1,
    modifications : Map.fromFoldable [Tuple "hours_per_week" 35, Tuple "days_per_week" 5]
}

to_half_time :: ExperimentalEvents
to_half_time = PermanentModificiation{
    effectDate : 2,
    modifications : Map.fromFoldable [Tuple "hours_per_week" 20]
}

low_season_worktime :: ExperimentalEvents
low_season_worktime = TempModificiation{
    effectDate : 3,
    endDate: 4,
    modifications : Map.fromFoldable [Tuple "hours_per_week" 10]
}

-- | events may be unordered (seq num are used here. We can think of dates, hash pointers to previous events...)
events :: Array ExperimentalEvents
events = [contract_creation, to_half_time, low_season_worktime ]
