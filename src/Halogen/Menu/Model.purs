module Halogen.Menu.Model where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.Menu.Submenu.Model (Submenu())

type Menu a =
  { chosen :: Maybe Int
  , submenus :: Array (MenuItem a)
  }

type MenuItem a = { label :: String, submenu :: Submenu a }

makeMenu :: forall a. Array (MenuItem a) -> Menu a
makeMenu submenus = { chosen: Nothing, submenus: submenus }

