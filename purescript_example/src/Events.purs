module Events where

import Prelude (class Ord)
import Data.Tuple (Tuple(..))
import Data.Map (Map, fromFoldable) 

-- | Events are generally indexed by their date or by a sequence number.
-- | Any type with full order can fit.
type EventTime a = (Ord a) => a

-- | Parameters are 
-- | Any type with full order can fit.
type Parameter = String

-- | Permanent or temporary amendment (or modifications)
data Modification eventtime parameter pvalues = 
    PermanentModificiation  { effectDate :: eventtime 
                            , modifications :: Map parameter pvalues
                            } 
    | TempModificiation     { effectDate :: eventtime 
                            , endDate :: eventtime
                            , modifications :: Map parameter pvalues
                            }

-- | V1, no checks of alloed parameters, validity of events, etc.
-- | Dates are ints (sequence number of events), parameters key and values are Strings.
type ExperimentalEvents = Modification Int String Int

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
    modifications : fromFoldable [Tuple "hours_per_week" 35, Tuple "days_per_week" 5]
}

to_half_time :: ExperimentalEvents
to_half_time = PermanentModificiation{
    effectDate : 2,
    modifications : fromFoldable [Tuple "hours_per_week" 20]
}

low_season_worktime :: ExperimentalEvents
low_season_worktime = TempModificiation{
    effectDate : 3,
    endDate: 4,
    modifications : fromFoldable [Tuple "hours_per_week" 10]
}
