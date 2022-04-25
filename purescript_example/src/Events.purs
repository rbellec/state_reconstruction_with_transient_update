module Events where

import Prelude (class Ord, class Show)
import Data.Tuple (Tuple(..))
import Data.Map (Map, keys)
import Data.Map as Map
import Data.Set (Set)
import Data.List (List, concatMap)
import Data.List as List

-- Generic derivations
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

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

-- | Generic derivations, this is purescript/haskell boilerplate code. Leave it if you do not know why this is here. 
derive instance genericModification :: Generic Modification
derive instance genericStateModificationType :: Generic StateModificationType
derive instance genericStateModification :: Generic StateModification _

instance showModification :: Show  Modification a b c where
  show = genericShow

instance showStateModificationType :: Show StateModificationType where
  show = genericShow

instance showStateModification :: Show StateModification a b c where
  show = genericShow




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
    

