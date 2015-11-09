module Halogen.Menu.Query where

-- | Query algebra for menu components.
data MenuQuery a next
  = SelectSubmenu Int next
  | DismissSubmenu next
