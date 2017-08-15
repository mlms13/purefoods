module App.Food where

import Control.Applicative (pure)
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.Generic.Rep (class Generic)
import Data.List.Types (List)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Prelude (bind, ($))

data FoodCategory
  = Protein
  | Dairy
  | Nut
  | Seeds
  | Legume
  | Grain
  | Vegetable
  | Fruit
  | Liquid
  | Tea
  | Misc
  | Sweetener
  | Spice
  | Medication

newtype Food = Food
  { name :: String
  , aliases :: List String
  , category :: FoodCategory
  , reflux :: Int
  , irritant :: Int
  , bacteria :: Int
  }


derive instance genericRepFoodCategory :: Generic FoodCategory _
instance decodeJsonFoodCategory :: DecodeJson FoodCategory where
  decodeJson json = do
    str <-  fromMaybe (Left "Failed to parse as string") (Right <$> toString json)
    case str of
      "Protein" -> Right Protein
      "Dairy" -> Right Dairy
      "Nut" -> Right Nut
      "Seeds" -> Right Seeds
      "Legume" -> Right Legume
      "Grain" -> Right Grain
      "Vegetable" -> Right Vegetable
      "Fruit" -> Right Fruit
      "Liquid" -> Right Liquid
      "Tea" -> Right Tea
      "Misc" -> Right Misc
      "Sweetener" -> Right Sweetener
      "Spice" -> Right Spice
      "Medication" -> Right Medication
      unk -> Left ("Unknown food category found: " <> unk)



derive instance genericRepFood :: Generic Food _
instance decodeJsonFood :: DecodeJson Food where
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    aliases <- obj .? "aliases"
    category <- obj .? "category"
    reflux <- obj .? "reflux"
    irritant <- obj .? "irritant"
    bacteria <- obj .? "bacteria"
    pure $ Food
     { name, aliases, category, reflux, irritant, bacteria }

