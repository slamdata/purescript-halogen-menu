module Halogen.Menu.Component.Query where

import Halogen.Menu.Component.State (Menu())

-- | Query algebra for menu components.
data MenuQuery a next
  = SetMenu (Menu a) next
  | SelectSubmenu Int next
  | DismissSubmenu next
