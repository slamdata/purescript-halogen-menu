module Halogen.Menu.Component.State where

import Data.Maybe (Maybe(..))

import Halogen.Menu.Submenu.Component.State (Submenu())

type Menu a =
  { chosen :: Maybe Int
  , submenus :: Array (MenuItem a)
  }

type MenuItem a = { label :: String, submenu :: Submenu a }

makeMenu :: forall a. Array (MenuItem a) -> Menu a
makeMenu submenus = { chosen: Nothing, submenus: submenus }

