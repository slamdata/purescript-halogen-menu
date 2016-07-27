module Halogen.Menu.Submenu.Component
  ( submenuComponent
  , module Halogen.Menu.Submenu.Component.State
  , module Halogen.Menu.Submenu.Component.Query
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.Menu.Submenu.Component.Query (SubmenuQuery(..))
import Halogen.Menu.Submenu.Component.State (Submenu, SubmenuItem)

type HTML a = H.ComponentHTML (SubmenuQuery a)
type DSL a g = H.ComponentDSL (Submenu a) (SubmenuQuery a) g

submenuComponent :: forall g a. H.Component (Submenu a) (SubmenuQuery a) g
submenuComponent = H.component { render, eval }
  where

  render :: Submenu a -> HTML a
  render = HH.ul_ <<< map renderItem

  renderItem :: SubmenuItem a -> HTML a
  renderItem item =
    HH.li_
      [ HH.a
          [ HE.onMouseUp $ HE.input_ (SelectSubmenuItem item.value) ]
          $ [ HH.span_ [ HH.text item.label ] ]
          <> renderShortcutLabel item.shortcutLabel
      ]

  renderShortcutLabel :: Maybe String -> Array (HTML a)
  renderShortcutLabel = maybe [] (\s -> [ HH.span_ [ HH.text s ] ])

  eval :: SubmenuQuery a ~> DSL a g
  eval (SelectSubmenuItem _ next) = pure next
