## Module Halogen.Menu.Component

#### `MenuP`

``` purescript
type MenuP a g = InstalledState (Menu a) (Submenu a) (MenuQuery a) (SubmenuQuery a) g SubmenuSlotAddress
```

Type synonym for the state of menu components.

The `a` parameter represents the type of value to be included in the
submenu component's `SelectSubmenuOption` query.

#### `MenuQueryP`

``` purescript
type MenuQueryP a = Coproduct (MenuQuery a) (ChildF SubmenuSlotAddress (SubmenuQuery a))
```

Type synonym for the query algebra of menu components.

The `a` parameter represents the type of value to be included in the
submenu component's `SelectSubmenuOption` query.

#### `SubmenuSlotAddress`

``` purescript
newtype SubmenuSlotAddress
  = SubmenuSlotAddress Int
```

Data type that represents the Halogen slot address of a specific submenu.

##### Instances
``` purescript
Generic SubmenuSlotAddress
Eq SubmenuSlotAddress
Ord SubmenuSlotAddress
```

#### `menuComponent`

``` purescript
menuComponent :: forall a g. (Functor g) => Component (MenuP a g) (MenuQueryP a) g
```

A Halogen component which presents an interactive menu.

##### HTML
Menus are rendered as unordered lists of submenus which can be selected and
dismissed. Selected submenus are also rendered as unordered lists.

Here is an example of a rendered menu with the first submenu selected.

```HTML
<ul><li><a>Color</a><ul><li><a><span>Load color</span></a></li><li><a><span>Save color</span></a></li></ul></li><li><div><a>Edit</a></div></li></ul>
```

##### State
An example of constructing the initial state of a menu component is
available
[here](https://github.com/beckyconning/color-editor/blob/master/src/ColorEditorMenu/Component/State.purs#L10).

##### Component
An example of installing a menu component is available
[here](https://github.com/beckyconning/color-editor/blob/master/src/ColorEditor/Component.purs#L72).

##### Query algebra
Selecting an item from a submenu will cause the submenu component to
evaluate a `SelectedSubmenuItem` query. This query contains a value which
is specified in the initial state of the menu.

This value may be of any type. The peek function of the component in which
the menu is installed (or its parents) may transform this value into
operations which modify component state or query other components.

If this value is specified as a query then it can be easily routed to
another component. This approach makes the intended effect of selecting a
submenu item quite clear. An example of this approach is available
[here](https://github.com/beckyconning/color-editor/blob/master/src/ColorEditor/Component.purs#L101).

##### Styling
Presented menus have no styling, ids or classes. To style a menu place it
inside of an element with a class or id and then provide styling for the
unordered lists, items and anchors inside it. An example stylesheet is
available
[here](https://github.com/beckyconning/color-editor/blob/master/stylesheet.css).

If you are providing keyboard shortcut labels to be rendered we recommend
using the flexible box model on the submenu item's anchor and a fifty
percent width for the spans inside it. If you can't use the flexible box
model we reccomend you define a fixed width for your submenus.


