module App.Data.Food where

import Control.Applicative (pure)
import Data.Argonaut.Core (toString)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Combinators ((.?), (.??))
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.Generic.Rep (class Generic)
import Data.List (groupBy)
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe, fromMaybe)
import Data.Semigroup ((<>))
import Prelude (class Eq, class Show, bind, ($), (==))
import Type.Data.Boolean (kind Boolean)

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
  , description :: Maybe String
  , aliases :: List String
  , category :: FoodCategory
  , reflux :: Int
  , irritant :: Int
  , bacteria :: Int
  }

byCategory :: List Food -> List (NonEmptyList Food)
byCategory =
  groupBy sameCat where
  sameCat (Food a) (Food b) = a.category == b.category

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
    description <- obj .?? "description"
    aliases <- obj .? "aliases"
    category <- obj .? "category"
    reflux <- obj .? "reflux"
    irritant <- obj .? "irritant"
    bacteria <- obj .? "bacteria"
    pure $ Food
     { name, description, aliases, category, reflux, irritant, bacteria }

derive instance eqFoodCategory :: Eq FoodCategory
instance showFoodCategory :: Show FoodCategory where
  show Protein = "Protein"
  show Dairy = "Dairy"
  show Nut = "Nut"
  show Seeds = "Seeds"
  show Legume = "Legume"
  show Grain = "Grain"
  show Vegetable = "Vegetable"
  show Fruit = "Fruit"
  show Liquid = "Liquid"
  show Tea = "Tea"
  show Misc = "Misc"
  show Sweetener = "Sweetener"
  show Spice = "Spice"
  show Medication = "Medication"
