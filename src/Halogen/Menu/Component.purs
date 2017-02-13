module Halogen.Menu.Component
  ( component
  , MenuItem
  , State
  , Query(..)
  , Message(..)
  ) where

import Prelude

import Control.Monad.Aff.Class (class MonadAff)

import Data.Array (mapWithIndex)

import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Event.Types as DET
import DOM.Event.Event as DEE

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Menu.Submenu.Component as SUB

data Query a next
  = Set (State a) next
  | SelectSubmenu Int next
  | HandleMouseEvent DET.MouseEvent (Query a next)
  | DismissSubmenu next
  | HandleSubmenu (SUB.Message a) next

type State a =
  { chosen ∷ Maybe Int
  , submenus ∷ Array (MenuItem a)
  }

type MenuItem a = { label ∷ String, submenu ∷ SUB.State a }

data Message a = Selected a

type HTML a m = H.ParentHTML (Query a) (SUB.Query a) Int m
type DSL a m = H.ParentDSL (State a) (Query a) (SUB.Query a) Int (Message a) m
type Input a = Array { label ∷ String, submenu ∷ SUB.State a }

component
  ∷ ∀ m a r
  . (MonadAff (dom ∷ DOM|r) m)
  ⇒ H.Component HH.HTML (Query a) (Input a) (Message a) m
component = H.parentComponent
  { initialState: \submenus → { chosen: Nothing, submenus }
  , receiver: const Nothing
  , eval
  , render
  }

render
  ∷ ∀ a m r
  . (MonadAff (dom ∷ DOM|r) m)
  ⇒ State a → HTML a m
render state =
  HH.ul_ $ mapWithIndex (renderSubmenu state) state.submenus
  where
  renderSubmenu ∷ State a → Int → MenuItem a → HTML a m
  renderSubmenu menu index submenu = case menu.chosen of
    Just ix | ix == index → renderChosenSubmenu ix submenu
    _ → renderHiddenSubmenu index submenu

  renderChosenSubmenu ∷ Int → MenuItem a → HTML a m
  renderChosenSubmenu ix submenu =
    HH.li_
      [ renderAnchor DismissSubmenu submenu.label
      , HH.slot ix SUB.component submenu.submenu $ HE.input HandleSubmenu
      ]

  renderHiddenSubmenu ∷ Int → MenuItem a → HTML a m
  renderHiddenSubmenu ix submenu =
    HH.li_ [ HH.div_ [ renderAnchor (SelectSubmenu ix) submenu.label ] ]

  renderAnchor ∷ H.Action (Query a) → String → HTML a m
  renderAnchor query label =
    HH.a
      [ HE.onClick \e → Just $ HandleMouseEvent e $ H.action query ]
      [ HH.text label ]

eval
  ∷ ∀ a m r
  . (MonadAff (dom ∷ DOM|r) m)
  ⇒ Query a ~> DSL a m
eval = case _ of
  HandleMouseEvent e query → do
    H.liftEff do
      DEE.preventDefault $ DET.mouseEventToEvent e
      DEE.stopPropagation $ DET.mouseEventToEvent e
    eval query
  SelectSubmenu ix next → do
    H.modify _{ chosen = Just ix }
    pure next
  DismissSubmenu next → do
    H.modify _{ chosen = Nothing }
    pure next
  Set state next → do
    H.put state
    pure next
  HandleSubmenu (SUB.Selected a) next → do
    H.raise $ Selected a
    pure next
