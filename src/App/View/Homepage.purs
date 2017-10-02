module App.View.Homepage where

import App.Events (Event)
import App.Food (Food(..))
import App.State (State(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (($))
import Data.Maybe (maybe)
import Data.Show (show)
import Data.Unit (Unit)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1, li, span, ul)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup (MarkupM, leaf, text, (!))

view :: State -> HTML Event
view (State { foods: Success foods }) =
  ul ! className "food-cards" $ do
    for_ foods $ foodLi

view (State { foods: Failure err }) =
  div $ text err

view s =
  div do
    h1 $ text "Pux"
    a ! className "guide" ! href "https://www.purefoods.org/" $ text "Guide"
    a ! className "github" ! href "https://github.com/mlms13/purefoods/" $ text "GitHub"


foodLi :: Food -> MarkupM (DOMEvent -> Event) Unit
foodLi (Food food) =
  li ! className "food-card" $ do
    div $ do
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
