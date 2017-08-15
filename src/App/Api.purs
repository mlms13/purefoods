module App.Api where

import App.AppEffects (AppEffects)
import Control.Applicative (pure)
import Control.Monad.Aff (Aff, attempt)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Either (Either(..), either)
import Data.FrontMatter.JsYaml (decodeYaml)
import Data.Show (show)
import Network.HTTP.Affjax (get)
import Network.RemoteData (RemoteData, fromEither)
import Prelude (bind, ($), (<<<))

loadFoods :: ∀ e a. DecodeJson a => Aff (AppEffects e) (RemoteData String a)
loadFoods = do
  r <- attempt $ get "https://raw.githubusercontent.com/mlms13/foods/master/src/data/foods.yml"
  let decode response = decodeYaml response.response
  let result = either (Left <<< show) decode r
  pure $ fromEither result

