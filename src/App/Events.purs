module App.Events where

import App.Api (loadFoods)
import App.AppEffects (AppEffects)
import App.Data.DietPhase (DietPhase)
import App.Data.Food (Food)
import App.Routes (Route)
import App.State (FoodFilter(..), State(..))
import Control.Applicative (pure)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Network.RemoteData (RemoteData(..))
import Prelude (bind, ($))
import Pux (EffModel, noEffects)

data Event
  = PageView Route
  | Content (RemoteData String (List Food))
  | Search String
  | Filter (Maybe DietPhase)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp eff (State st @ {foods: NotAsked }) =
  { state: State st { foods = Loading }
  , effects:
    [pure $ Just eff, pure $ Just (Content NotAsked)]
  }
foldp (Content NotAsked) (State st) =
  { state: State st { foods = Loading }
  , effects: [ do
      content <- loadFoods
      pure $ Just $ Content content
    ]
  }
foldp (Content val) (State st) = noEffects $ State st { foods = val }

foldp (Search str) (State st) =
  noEffects $ State st { filter = filter} where
  (FoodFilter curr) = st.filter
  filter = FoodFilter curr { search = str }

foldp (Filter phase) (State st) =
  noEffects $ State st { filter = filter } where
  (FoodFilter curr) = st.filter
  filter = FoodFilter curr { phase = phase }

foldp (PageView route) (State st) = noEffects $ State st { route = route }
