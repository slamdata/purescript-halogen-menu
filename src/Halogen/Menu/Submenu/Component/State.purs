module Halogen.Menu.Submenu.Component.State where

import Data.Maybe (Maybe)

type Submenu a = Array (SubmenuItem a)

type SubmenuItem a =
  { label :: String
  , shortcutLabel :: Maybe String
  , value :: a
  }
