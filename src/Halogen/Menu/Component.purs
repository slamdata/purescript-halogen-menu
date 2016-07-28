module Halogen.Menu.Component
  ( MenuP
  , MenuQueryP
  , SubmenuSlotAddress(..)
  , menuComponent
  , module Halogen.Menu.Component.State
  , module Halogen.Menu.Component.Query
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Functor.Coproduct (Coproduct)
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Handler as HEH
import Halogen.Menu.Component.Query (MenuQuery(..))
import Halogen.Menu.Component.State (Menu, MenuItem)
import Halogen.Menu.Submenu.Component (submenuComponent)
import Halogen.Menu.Submenu.Component.Query (SubmenuQuery(..))
import Halogen.Menu.Submenu.Component.State (Submenu)

-- | Type synonym for the state of menu components.
-- |
-- | The `a` parameter represents the type of value to be included in the
-- | submenu component's `SelectSubmenuOption` query.
type MenuP a g = H.ParentState (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress

-- | Type synonym for the query algebra of menu components.
-- |
-- | The `a` parameter represents the type of value to be included in the
-- | submenu component's `SelectSubmenuOption` query.
type MenuQueryP a = Coproduct (MenuQuery a) (H.ChildF SubmenuSlotAddress (SubmenuQuery a))

-- | Data type that represents the Halogen slot address of a specific submenu.
newtype SubmenuSlotAddress = SubmenuSlotAddress Int

derive instance genericSubmenuSlotAddress :: Generic SubmenuSlotAddress
derive instance eqSubmenuSlotAddress :: Eq SubmenuSlotAddress
derive instance ordSubmenuSlotAddress :: Ord SubmenuSlotAddress

type HTML a g = H.ParentHTML (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress
type DSL a g = H.ParentDSL (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress

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
menuComponent :: forall a g. Functor g => H.Component (MenuP a g) (MenuQueryP a) g
menuComponent = H.parentComponent { render, eval, peek: Just (peek <<< H.runChildF) }

render :: forall a g. Menu a -> HTML a g
render menu =
  HH.ul_ $ mapWithIndex (renderSubmenu menu) menu.submenus

renderSubmenu :: forall a g. Menu a -> Int -> MenuItem a -> HTML a g
renderSubmenu menu index menuSubmenu
  | menu.chosen == Just index = renderChosenSubmenu index menuSubmenu
  | otherwise = renderHiddenSubmenu index menuSubmenu

renderChosenSubmenu :: forall a g. Int -> MenuItem a -> HTML a g
renderChosenSubmenu index menuSubmenu =
  HH.li_
    [ renderAnchor DismissSubmenu menuSubmenu.label
    , HH.slot (SubmenuSlotAddress index) \_ ->
        { component: submenuComponent
        , initialState: menuSubmenu.submenu
        }
    ]

renderHiddenSubmenu :: forall a g. Int -> MenuItem a -> HTML a g
renderHiddenSubmenu index menuSubmenu =
  HH.li_ [ HH.div_ [ renderAnchor (SelectSubmenu index) menuSubmenu.label ] ]

renderAnchor :: forall a g. H.Action (MenuQuery a) -> String -> HTML a g
renderAnchor a label =
  HH.a
    [ HE.onClick (\_ -> HEH.preventDefault *> HEH.stopPropagation $> Just (H.action a)) ]
    [ HH.text $ label ]

eval :: forall a g. MenuQuery a ~> DSL a g
eval (SelectSubmenu index next) = H.modify (_ { chosen = Just index }) $> next
eval (DismissSubmenu next) = H.modify (_ { chosen = Nothing }) $> next
eval (SetMenu menu next) = H.set menu $> next

peek :: forall a g x. SubmenuQuery a x -> DSL a g Unit
peek (SelectSubmenuItem _ _) = H.modify (_ { chosen = Nothing }) *> pure unit
