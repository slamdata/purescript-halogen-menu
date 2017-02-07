module Halogen.Menu.Submenu.Component
  ( component
  , State
  , Query(..)
  , Message(..)
  , SubmenuItem
  ) where

import Prelude

import Data.Array as Arr
import Data.Foldable as F
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML as HH

type State a = Array (SubmenuItem a)

type SubmenuItem a =
  { label ∷ String
  , shortcutLabel ∷ Maybe String
  , value ∷ a
  }

data Query a next
  = Select a next
  | SetState (State a) next

data Message a = Selected a

type HTML a = H.ComponentHTML (Query a)
type DSL a g = H.ComponentDSL (State a) (Query a) (Message a) g

component ∷ ∀ m a. H.Component HH.HTML (Query a) (State a) (Message a) m
component = H.component
  { render
  , eval
  , initialState: id
  , receiver: Just <<< H.action <<< SetState
  }

render ∷ ∀ a. State a → HTML a
render = HH.ul_ <<< map renderItem
  where
  renderItem ∷ SubmenuItem a → HTML a
  renderItem item =
    HH.li_
      [ HH.a
          [ HE.onMouseUp $ HE.input_ (Select item.value) ]
          $ [ HH.span_ [ HH.text item.label ] ]
          <> renderShortcutLabel item.shortcutLabel
      ]
  renderShortcutLabel ∷ Maybe String → Array (HTML a)
  renderShortcutLabel =
    F.foldMap $ Arr.singleton <<< HH.span_ <<< Arr.singleton <<< HH.text

eval ∷ ∀ a g. Query a ~> DSL a g
eval = case _ of
  Select val next → do
    H.raise $ Selected val
    pure next
  SetState st next → do
    H.put st
    pure next
