module App.View.Homepage where

import App.Data.Food (Food(..), byCategory)
import App.Events (Event)
import App.State (State(..), applyFilter)
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (($))
import Data.List.NonEmpty (head)
import Data.List.Types (NonEmptyList)
import Data.Maybe (maybe)
import Data.Monoid (mempty)
import Data.Show (show)
import Data.Unit (Unit)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (div, li, span, ul, h2)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (MarkupM, leaf, text, (!))

view :: State -> HTML Event
view (State { foods: Success all, filter }) =
  let
    foods = byCategory $ applyFilter filter all
  in
  ul ! className "container food-categories" $ do
    for_ foods viewCategoryLi

view (State { foods: Failure err }) =
  div $ text err

view _ =
  div mempty


viewCategoryLi :: NonEmptyList Food -> MarkupM (DOMEvent -> Event) Unit
viewCategoryLi foods =
  let Food({ category }) = (head foods)
  in
  li ! className "food-category" $ do
    h2 ! className "food-category-title" $ text (show category)
    ul ! className "food-cards" $ do
      for_ foods viewFoodLi

viewFoodLi :: Food -> MarkupM (DOMEvent -> Event) Unit
viewFoodLi (Food food) =
  li ! className "food-card" $ do
    div ! className "food-card-text" $ do
      span ! className "food-name" $ text food.name
      maybe (text "") (\s -> span ! className "food-preparation" $ text s) food.description
    div ! className "food-numbers" $ do
      div ! className "food-number-reflux" $ do
        leaf "i" ! className "food-card-icon icon-reflux"
        text $ show food.reflux
      div ! className "food-number-irritant" $ do
        leaf "i" ! className "food-card-icon icon-irritant"
        text $ show food.irritant
      div ! className "food-number-bacteria" $ do
        leaf "i" ! className "food-card-icon icon-bacteria"
        text $ show food.bacteria
