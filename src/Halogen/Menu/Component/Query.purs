module Halogen.Menu.Component.Query where

-- | Query algebra for menu components.
data MenuQuery a next
  = SelectSubmenu Int next
  | DismissSubmenu next
