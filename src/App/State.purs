module App.State where

import App.Data.DietPhase (DietPhase, Tolerance(..), tolerance)
import App.Data.Food (Food(..))
import App.Routes (Route, match)
import Data.Array (any, fromFoldable)
import Data.Foldable (all)
import Data.List (List, filter)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), contains, split, toLower)
import Network.RemoteData (RemoteData(..))
import Prelude (($), (&&), (<$>), (<<<), (<=), (<>))

newtype FoodFilter = FoodFilter
  { phase :: Maybe DietPhase
  , search :: String
  }

newtype State = State
  { route :: Route
  , foods :: RemoteData String (List Food)
  , filter :: FoodFilter
  }

applyFilter :: FoodFilter -> List Food -> List Food
applyFilter (FoodFilter { phase, search }) foods =
  filter pred foods where

  foodInTolerance :: Food -> DietPhase -> Boolean
  foodInTolerance (Food { reflux, irritant, bacteria }) dp =
    let (Tolerance t) = tolerance dp
    in reflux <= t.reflux && irritant <= t.irritant && bacteria <= t.bacteria

  matchesString :: Food -> Boolean
  matchesString (Food { name, description, aliases }) =
    let splitOnSpace = split $ Pattern " "
        terms = splitOnSpace $ toLower search
        validParts = splitOnSpace (toLower name) <> maybe [] (splitOnSpace <<< toLower) description <> (toLower <$> fromFoldable aliases)

        containsTerm :: String -> Boolean
        containsTerm t =
          any (contains $ Pattern t) validParts
    in
    all containsTerm terms

  pred :: Food -> Boolean
  pred food =
    maybe true (foodInTolerance food) phase && matchesString food


derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { route: match url
  , foods: NotAsked
  , filter: FoodFilter
    { phase: Nothing, search: "" }
  }
