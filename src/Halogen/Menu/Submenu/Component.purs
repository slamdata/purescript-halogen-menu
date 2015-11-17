module Halogen.Menu.Submenu.Component
  ( submenuComponent
  , module Halogen.Menu.Submenu.Component.State
  , module Halogen.Menu.Submenu.Component.Query
  ) where

import Prelude

import Data.Maybe (maybe)
import Data.Void (Void())

import Halogen
import qualified Halogen.HTML.Events.Indexed as E
import qualified Halogen.HTML.Indexed as H

import Halogen.Menu.Submenu.Component.State (Submenu(), SubmenuItem())
import Halogen.Menu.Submenu.Component.Query (SubmenuQuery(..))

submenuComponent :: forall g a. Component (Submenu a) (SubmenuQuery a) g
submenuComponent = component render eval
  where

  render :: Render (Submenu a) (SubmenuQuery a)
  render = H.ul_ <<< map renderItem

  renderItem :: (SubmenuItem a) -> HTML Void (SubmenuQuery a)
  renderItem item =
    H.li_
      [ H.button
          [ E.onClick $ E.input_ (SelectSubmenuItem item.value) ]
          [ H.text item.label ]
      ]

  eval :: Eval (SubmenuQuery a) (Submenu a) (SubmenuQuery a) g
  eval (SelectSubmenuItem _ next) = pure next
