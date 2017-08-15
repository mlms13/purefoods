module App.State where

import App.Routes (Route, match)
import App.Food (Food)
import Data.List (List)
import Data.Newtype (class Newtype)
import Network.RemoteData (RemoteData(..))


newtype State = State
  { route :: Route
  , foods :: RemoteData String (List Food)
  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { route: match url
  , foods: NotAsked
  }
