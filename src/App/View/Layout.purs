module App.View.Layout where

import App.Events (Event)
import App.Routes (Route(NotFound, Home))
import App.State (State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, color, fromString, key, marginLeft, px, value, (?))
import CSS.Background (backgroundColor)
import CSS.Border (border, solid)
import CSS.Box (borderBox, boxShadow, boxSizing)
import CSS.Display (display, flex)
import CSS.Flexbox (flexWrap, justifyContent, spaceAround, wrap)
import CSS.Font (fontSize)
import CSS.Geometry (marginBottom, marginRight, marginTop, padding, width)
import CSS.ListStyle.Type (ListStyleType(..), listStyleType)
import CSS.Size (pct)
import CSS.Text (noneTextDecoration, textDecoration, underline)
import Color (darken, lighten, rgb, rgba)
import Color.Scheme.Clrs (green, red)
import Control.Bind (discard)
import Data.Function (($))
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css

    case st.route of
      (Home) -> Homepage.view (State st)
      (NotFound url) -> NotFound.view (State st)

css :: CSS
css = do
  let white = rgb 255 255 255
      -- gray and shades
      gray = rgb 130 130 130
      grayLightest = lighten 0.4 gray
      grayLighter = lighten 0.2 gray
      grayLight = lighten 0.1 gray
      grayDark = darken 0.1 gray
      grayDarker = darken 0.2 gray
      grayDarkest = darken 0.3 gray

      -- primary/accent and shades
      blue = rgb 20 40 230
      primary = blue
      primaryLight = lighten 0.1 primary

      -- shorthand helpers
      paddingAll amt = padding (px amt) (px amt) (px amt) (px amt)

  fromString "*" ? do
    boxSizing borderBox

  fromString "body" ? do
    backgroundColor grayLightest
    key (fromString "font-family") (value "-apple-system,BlinkMacSystemFont,\"Segoe UI\",Roboto,Oxygen-Sans,Ubuntu,Cantarell,\"Helvetica Neue\",sans-serif")

  fromString "a" ? do
    color primary
    textDecoration noneTextDecoration

  fromString "a:hover" ? do
    color primaryLight
    textDecoration underline

  fromString ".food-cards" ? do
    display flex
    flexWrap wrap
    justifyContent spaceAround
    listStyleType None

  fromString ".food-card" ? do
    backgroundColor white
    border solid (px 1.0) grayLighter
    boxShadow (px 1.0) (px 1.0) (px 5.0) $ rgba 0 0 0 0.2
    marginBottom $ px 20.0
    paddingAll 20.0
    width $ pct 30.0

  fromString ".food-name" ? do
    color grayDarkest

  fromString ".food-preparation" ? do
    color gray
    marginLeft $ px 8.0

  fromString ".food-numbers" ? do
    display flex
    fontSize $ px 26.0
    justifyContent spaceAround
    marginTop $ px 10.0

  fromString ".food-number-reflux" ? do
    color red

  fromString ".food-number-irritant" ? do
    color blue

  fromString ".food-number-bacteria" ? do
    color green

  fromString ".food-card-icon" ? do
    marginRight $ px 10.0
