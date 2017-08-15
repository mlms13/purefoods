module App.View.Homepage where

import App.Events (Event)
import App.Food (Food(..))
import App.State (State(..))
import Control.Bind (discard)
import Data.Foldable (for_)
import Data.Function (($))
import Data.Unit (Unit)
import Network.RemoteData (RemoteData(..))
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, div, h1, li, ul)
import Text.Smolder.HTML.Attributes (href, className)
import Text.Smolder.Markup (MarkupM, text, (!))

view :: State -> HTML Event
view (State { foods: Success foods }) =
  ul do
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
  li $ text food.name
