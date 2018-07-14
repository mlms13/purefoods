module App.View.Layout where

import App.Data.DietPhase (DietPhase(..))
import App.Events (Event(..))
import App.Routes (Route(NotFound, Home))
import App.State (FoodFilter(..), State(..))
import App.View.Homepage as Homepage
import App.View.NotFound as NotFound
import CSS (CSS, block, bold, border, borderBottom, color, column, fixed, flexDirection, flexGrow, fontFamily, fontWeight, fromString, key, left, lineHeight, margin, marginLeft, maxWidth, minHeight, paddingBottom, paddingLeft, paddingRight, paddingTop, position, px, right, sansSerif, solid, top, (?))
import CSS.Background (backgroundColor)
import CSS.Box (borderBox, boxSizing)
import CSS.Common (auto)
import CSS.Display (display, flex)
import CSS.Flexbox (flexWrap, justifyContent, spaceAround, wrap)
import CSS.Font (fontSize)
import CSS.Geometry (marginBottom, marginRight, marginTop, padding, width)
import CSS.ListStyle.Type (ListStyleType(..), listStyleType)
import CSS.Size (pct)
import CSS.Text (noneTextDecoration, textDecoration, underline)
import Color (darken, lighten, rgb)
import Control.Bind (discard)
import Data.Function (($), (<<<))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (singleton)
import Prelude (show, (<>), (==))
import Pux.DOM.Events (onClick, onKeyUp, targetValue)
import Pux.DOM.HTML (HTML, style)
import Text.Smolder.HTML (div, header, input)
import Text.Smolder.HTML.Attributes (className, placeholder, type', value)
import Text.Smolder.Markup (text, (!), (#!))

view :: State -> HTML Event
view (State st) =
  div ! className "app" $ do
    style css
    viewHeader st.filter
    viewBody $ State st

viewHeader :: FoodFilter -> HTML Event
viewHeader (FoodFilter { search, phase }) =
  header ! className "app-header" $ do
    div ! className "container d-flex" $ do
      input ! className "search-input" ! type' "text" ! placeholder "Filter" ! value search #! onKeyUp (Search <<< targetValue)
      viewPhaseToggle phase

viewPhaseToggle :: Maybe DietPhase -> HTML Event
viewPhaseToggle phase =
  div ! className "phase-toggle" $ do
    div ! className "phase-toggle-label" $ do text "Phase:"
    btn One
    btn Two
    btn Three

  where
    btn :: DietPhase -> HTML Event
    btn dp =
      let active = phase == Just dp
      in
      div
        ! className ("phase-toggle-btn" <> if active then " active" else "")
        #! onClick (\_ ->  if active then (Filter Nothing) else (Filter $ Just dp))
        $ do text $ show dp


viewBody :: State -> HTML Event
viewBody (State st) =
  case st.route of
    Home -> Homepage.view (State st)
    NotFound url -> NotFound.view (State st)

css :: CSS
css = do
  let white = rgb 255 255 255
      -- gray and shades
      gray = rgb 130 130 130
      grayLightest = rgb 244 244 244
      grayLighter = lighten 0.4 gray
      grayLight = lighten 0.3 gray
      grayDark = darken 0.1 gray
      grayDarker = darken 0.2 gray
      grayDarkest = darken 0.3 gray

      -- primary/accent and shades
      blue = rgb 20 160 210
      primary = blue
      primaryLight = lighten 0.3 primary

      -- shorthand helpers
      paddingAll amt = padding (px amt) (px amt) (px amt) (px amt)

  fromString "*" ? do
    boxSizing borderBox

  fromString "body" ? do
    backgroundColor grayLighter
    fontFamily ["Lato"] $ singleton sansSerif
    paddingAll 0.0
    paddingTop (px 60.0)
    margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)

  fromString "a" ? do
    color primary
    textDecoration noneTextDecoration

  fromString "a:hover" ? do
    color primaryLight
    textDecoration underline

  fromString ".container" ? do
    maxWidth $ px 1100.0
    marginRight auto
    marginLeft auto

  fromString ".d-flex" ? do
    display flex

  fromString ".app-header" ? do
    backgroundColor grayDarker
    paddingTop $ px 20.0
    paddingBottom $ px 20.0
    position fixed
    top $ px 0.0
    left $ px 0.0
    right $ px 0.0


  fromString ".search-input" ? do
    border solid (px 0.0) white
    flexGrow 1
    fontSize $ px 16.0
    padding (px 6.0) (px 10.0) (px 6.0) (px 10.0)

  fromString ".phase-toggle" ? do
    color grayLighter
    display flex
    lineHeight $ px 30.0
    marginLeft $ px 20.0

  fromString ".phase-toggle-btn" ? do
    color white
    key (fromString "cursor") "pointer"
    paddingLeft $ px 5.0
    paddingRight $ px 5.0
    marginRight $ px 5.0
    marginLeft $ px 5.0

  fromString ".phase-toggle-btn.active" ? do
    color primaryLight
    borderBottom solid (px 2.0) primaryLight

  fromString ".food-categories" ? do
    listStyleType None
    paddingLeft $ px 0.0

  fromString ".food-category-title" ? do
    borderBottom solid (px 2.0) grayLight
    fontFamily ["Catamaran"] $ singleton sansSerif
    fontSize $ px 30.0
    fontWeight bold

  fromString ".food-cards" ? do
    display flex
    flexWrap wrap
    listStyleType None
    paddingLeft $ px 0.0

  fromString ".food-card" ? do
    backgroundColor white
    display flex
    flexDirection column
    marginBottom $ px 20.0
    marginRight $ pct 2.0
    minHeight $ px 130.0
    width $ pct 32.0

  fromString ".food-card:nth-child(3n)" ? do
    marginRight $ pct 0.0

  fromString ".food-card-text" ? do
    padding (px 12.0) (px 20.0) (px 0.0) (px 20.0)

  fromString ".food-name" ? do
    color grayDarker
    fontFamily ["Catamaran"] $ singleton sansSerif
    fontSize $ px 24.0
    fontWeight bold

  fromString ".food-preparation" ? do
    color grayLight
    display block
    fontSize $ px 14.0

  fromString ".food-numbers" ? do
    backgroundColor grayLightest
    color gray
    display flex
    fontSize $ px 18.0
    justifyContent spaceAround
    marginTop auto

  fromString ".food-number-reflux" ? do
    paddingAll 10.0

  fromString ".food-number-irritant" ? do
    paddingAll 10.0

  fromString ".food-number-bacteria" ? do
    paddingAll 10.0

  fromString ".food-card-icon" ? do
    marginRight $ px 10.0
