module Halogen.Menu.Submenu.Component.State where

import Prelude

import Data.Maybe (Maybe())

type Submenu a = Array (SubmenuItem a)

type SubmenuItem a =
  { label :: String
  , value :: a
  }

