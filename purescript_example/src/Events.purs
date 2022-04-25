module Events where

-- import Prelude (class Ord, class Show)
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
-- | Any type with full order can fit. This version is simplified to use Ints

type EventDate = Int
type ParameterName = String
type ParameterValue = Int

-- | Permanent or temporary amendment (or modifications)
--   This type uses String & ints in order to demonstrate the feature and
--   should be made more generic once done.
data Modification = 
    PermanentModificiation  { effectDate :: EventDate 
                            , modifications :: Map ParameterName ParameterValue
                            } 
    | TempModificiation     { effectDate :: EventDate 
                            , endDate :: EventDate
                            , modifications :: Map ParameterName ParameterValue
                            }

data StateModificationType = Apply | ApplyTransient | ReleaseTransient

type StateModification = { 
      effectDate :: EventDate 
    , eventType :: StateModificationType
    , parameters :: Set ParameterName
    , sourceEvent :: Modification 
    } 

-- | V1, no checks of allowed parameters, validity of events, etc.
-- | Dates are ints (sequence number of events), parameters key and values are Strings.


-- | Generic derivations, this is purescript/haskell boilerplate code. Leave it if you do not know why this is here. 
-- derive instance genericModification :: Generic Modification
-- derive instance genericStateModificationType :: Generic StateModificationType
-- derive instance genericStateModification :: Generic StateModification 

-- instance showModification :: Show  Modification where
--   show = genericShow

-- instance showStateModificationType :: Show StateModificationType where
--   show = genericShow

-- instance showStateModification :: Show StateModification where
--   show = genericShow




-- Todo : test purescript optics
-- Todo : find `$` equiv in purescript
-- Todo : explicit multiple syntax tested here to explain this part of purescript.
-- parameters_modified_in :: forall a b parameter. (Modification a parameter b) -> Set parameter
parameters_modified_in :: Modification -> Set ParameterName
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
    

