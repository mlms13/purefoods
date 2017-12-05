module App.State where

import App.Data.DietPhase (DietPhase)
import App.Data.Food (Food)
import App.Routes (Route, match)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Network.RemoteData (RemoteData(..))

newtype FoodFilter = FoodFilter
  { phase :: Maybe DietPhase
  , search :: String
  }

newtype State = State
  { route :: Route
  , foods :: RemoteData String (List Food)
  , filter :: FoodFilter
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { route: match url
  , foods: NotAsked
  , filter: FoodFilter
    { phase: Nothing, search: "" }
  }
