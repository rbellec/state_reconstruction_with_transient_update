module Events where

import Prelude (class Ord)
import Data.Tuple (Tuple(..))
import Data.Map (Map, keys)
import Data.Map as Map
import Data.Set (Set)
import Data.List (List, concatMap)
import Data.List as List

-- | Events are generally indexed by their date or by a sequence number.
-- | Any type with full order can fit.
type EventTime a = (Ord a) => a

-- | Parameters are 
-- | Any type with full order can fit.
type Parameter = String

-- | Permanent or temporary amendment (or modifications)
data Modification event_time parameter pvalues = 
    PermanentModificiation  { effectDate :: event_time 
                            , modifications :: Map parameter pvalues
                            } 
    | TempModificiation     { effectDate :: event_time 
                            , endDate :: event_time
                            , modifications :: Map parameter pvalues
                            }

data StateModificationType = Apply | ApplyTransient | ReleaseTransient

type StateModification event_time parameter pvalues = { 
      effectDate :: event_time 
    , eventType :: StateModificationType
    , parameters :: Set parameter
    , sourceEvent :: Modification event_time parameter pvalues
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

--  ------------------------------------------------------------------------------------------
--  PermanentModificiation  { effectDate :: eventtime 
--                             , modifications :: Map parameter pvalues
--                             } 
--     |      { effectDate :: eventtime 
--                             , endDate :: eventtime
--                             , modifications :: Map parameter pvalues
--                             }

-- data StateModificationType = Apply | ApplyTransient | ReleaseTransient

--  ------------------------------------------------------------------------------------------
-- Todo : test purescript optics
-- Todo : find `$` equiv in purescript
-- Todo : explicit multiple syntax tested here to explain this part of purescript.
-- parameters_modified_in :: forall a b parameter. (Modification a parameter b) -> Set parameter
parameters_modified_in :: forall parameter. (Modification _ parameter _) -> Set parameter
parameters_modified_in (PermanentModificiation a) = keys a.modifications
parameters_modified_in (TempModificiation a) = keys (_.modifications a)


-- | Create ordered list of state modifications from a single event.
-- events_from_modification :: Modification a b c -> List StateModification a b c
events_from_modification amendment =
    let 
        params = parameters_modified_in amendment

        events = 
            case amendment of
            PermanentModificiation perm -> [  
                                                { effectDate: perm.effectDate
                                                , eventType : Apply
                                                , parameters : params
                                                , sourceEvent : amendment
                                                }
                                            ]  
            TempModificiation transient -> [ 
                                                { eventType : ApplyTransient
                                                , effectDate: transient.effectDate
                                                , parameters : params
                                                , sourceEvent : amendment
                                                },

                                                { eventType : ReleaseTransient
                                                , effectDate: transient.endDate
                                                , parameters : params
                                                , sourceEvent : amendment
                                                }
                                            ] 
    in 
    List.fromFoldable events

-- | 
state_modifications_from = concatMap events_from_modification 
    

