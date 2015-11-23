module Halogen.Menu.Component
  ( menuComponent
  , MenuP()
  , MenuQueryP()
  , SubmenuSlotAddress(..)
  , menuComponent
  , module Halogen.Menu.Component.State
  , module Halogen.Menu.Component.Query
  ) where

import Prelude

import Control.Apply ((*>))
import Data.Array (zip, (..), length)
import Data.Functor (($>))
import Data.Functor.Coproduct (Coproduct())
import Data.Generic (Generic, gEq, gCompare)
import Data.Maybe (Maybe(..))
import Data.NaturalTransformation (Natural())
import Data.Tuple (Tuple(..))

import Halogen
import qualified Halogen.HTML.Events as E
import qualified Halogen.HTML as H
import qualified Halogen.HTML.Events.Handler as EH

import Halogen.Menu.Component.State (Menu(), MenuItem())
import Halogen.Menu.Component.Query (MenuQuery(..))
import Halogen.Menu.Submenu.Component (submenuComponent)
import Halogen.Menu.Submenu.Component.State (Submenu())
import Halogen.Menu.Submenu.Component.Query (SubmenuQuery(..))

-- | Type synonym for the state of menu components.
-- |
-- | The `a` parameter represents the type of value to be included in the
-- | submenu component's `SelectSubmenuOption` query.
type MenuP a g = InstalledState (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress

-- | Type synonym for the query algebra of menu components.
-- |
-- | The `a` parameter represents the type of value to be included in the
-- | submenu component's `SelectSubmenuOption` query.
type MenuQueryP a = Coproduct (MenuQuery a) (ChildF SubmenuSlotAddress (SubmenuQuery a))

-- | Data type that represents the Halogen slot address of a specific submenu.
newtype SubmenuSlotAddress = SubmenuSlotAddress Int

derive instance genericSubmenuSlotAddress :: Generic SubmenuSlotAddress
instance eqSubmenuSlotAddress :: Eq SubmenuSlotAddress where eq = gEq
instance ordSubmenuSlotAddress :: Ord SubmenuSlotAddress where compare = gCompare

-- | A Halogen component which presents an interactive menu.
-- |
-- | ##### HTML
-- | Menus are rendered as unordered lists of submenus which can be selected and
-- | dismissed. Selected submenus are also rendered as unordered lists.
-- |
-- | Here is an example of a rendered menu with the first submenu selected.
-- |
-- | ```HTML
-- | <ul><li><a>Color</a><ul><li><a><span>Load color</span></a></li><li><a><span>Save color</span></a></li></ul></li><li><div><a>Edit</a></div></li></ul>
-- | ```
-- |
-- | ##### State
-- | An example of constructing the initial state of a menu component is
-- | available
-- | [here](https://github.com/beckyconning/color-editor/blob/master/src/ColorEditorMenu/Component/State.purs#L10).
-- |
-- | ##### Component
-- | An example of installing a menu component is available
-- | [here](https://github.com/beckyconning/color-editor/blob/master/src/ColorEditor/Component.purs#L72).
-- |
-- | ##### Query algebra
-- | Selecting an item from a submenu will cause the submenu component to
-- | evaluate a `SelectedSubmenuItem` query. This query contains a value which
-- | is specified in the initial state of the menu.
-- |
-- | This value may be of any type. The peek function of the component in which
-- | the menu is installed (or its parents) may transform this value into
-- | operations which modify component state or query other components.
-- |
-- | If this value is specified as a query then it can be easily routed to
-- | another component. This approach makes the intended effect of selecting a
-- | submenu item quite clear. An example of this approach is available
-- | [here](https://github.com/beckyconning/color-editor/blob/master/src/ColorEditor/Component.purs#L101).
-- |
-- | ##### Styling
-- | Presented menus have no styling, ids or classes. To style a menu place it
-- | inside of an element with a class or id and then provide styling for the
-- | unordered lists, items and anchors inside it. An example stylesheet is
-- | available
-- | [here](https://github.com/beckyconning/color-editor/blob/master/stylesheet.css).
-- |
-- | If you are providing keyboard shortcut labels to be rendered we recommend
-- | using the flexible box model on the submenu item's anchor and a fifty
-- | percent width for the spans inside it. If you can't use the flexible box
-- | model we reccomend you define a fixed width for your submenus.

menuComponent :: forall a g. (Functor g) => Component (MenuP a g) (MenuQueryP a) g
menuComponent = parentComponent' render eval peek
  where

  render :: Menu a -> ParentHTML (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress
  render menu = H.ul_ $ map renderSubmenu (zip (0 .. (length menu.submenus - 1)) menu.submenus)
    where

    renderSubmenu :: Tuple Int (MenuItem a) -> ParentHTML (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress
    renderSubmenu (Tuple index menuSubmenu)
      | menu.chosen == Just index = renderChosenSubmenu index menuSubmenu
      | otherwise = renderHiddenSubmenu index menuSubmenu

    renderChosenSubmenu :: Int -> MenuItem a -> ParentHTML (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress
    renderChosenSubmenu index menuSubmenu =
      H.li_
        [ renderAnchor DismissSubmenu menuSubmenu.label
        , H.slot (SubmenuSlotAddress index) \_ ->
            { component: submenuComponent
            , initialState: menuSubmenu.submenu
            }
        ]

    renderHiddenSubmenu :: Int -> MenuItem a -> ParentHTML (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress
    renderHiddenSubmenu index menuSubmenu =
      H.li_ [ H.div_ [ renderAnchor (SelectSubmenu index) menuSubmenu.label ] ]

    renderAnchor :: forall f p. Action f -> String -> HTML p f
    renderAnchor a label =
      H.a
        [ E.onClick (\_ -> EH.preventDefault *> EH.stopPropagation $> a unit) ]
        [ H.text $ label ]

  eval :: Natural (MenuQuery a) (ParentDSL (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress)
  eval (SelectSubmenu index next) = modify (_ { chosen = Just index }) $> next
  eval (DismissSubmenu next) = modify (_ { chosen = Nothing }) $> next

  peek :: forall i. ChildF SubmenuSlotAddress (SubmenuQuery a) i -> ParentDSL (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress Unit
  peek (ChildF _ (SelectSubmenuItem _ _)) = modify (_ { chosen = Nothing }) *> pure unit
