# purescript-halogen-menu

[![Latest release](http://img.shields.io/github/release/slamdata/purescript-halogen-menu.svg)](https://github.com/slamdata/purescript-halogen-menu/releases)
[![Build status](https://travis-ci.org/slamdata/purescript-halogen-menu.svg?branch=master)](https://travis-ci.org/slamdata/purescript-halogen-menu)

A reusable halogen component that presents an interactive menu.

## HTML
Menus are rendered as unordered lists of submenus which can be selected and dismissed. Selected submenus are also rendered as unordered lists.

Here is an example of a rendered menu with the first submenu selected.

```HTML
<ul><li><button>Color</button><ul><li><button>Load color</button></li><li><button>Save color</button></li></ul></li><li><div><button>Edit</button></div></li></ul>
```

## Guide

### Installing `purescript-halogen-menu`
* Install `purescript-markdown-halogen` using bower: `bower i purescript-markdown-halogen --save`.

### Rendering a menu
* Create a Halogen parent component to install your menu in.
* Decide what you want selecting an item from your menu to do. Commonly this will be to cause some other component or to evaluate a query.
* Define a query or other data type which represents what you want selecting an item from your menu to do.
* Import `Halogen.Menu.Model (Menu(), mkMenu)`.
* Construct the intitial state of your menu as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditorMenu/Model.purs) replacing `ColorQuery Unit` with the type you defined in the previous step.
* Import `Halogen.Menu.Component (MenuP(), MenuQueryP(), SubmenuSlotAddress(), menuComponent)`.
* Define a slot address for your menu.
* Define a type synonym for your menu's state and queries using the query or other data type you defined above as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditor/Component.purs#L37-L38).
* Use these type synonyms and slot address type to redefine the child state, child query and child slot address type synonyms for your parent component as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditor/Component.purs#L51-L53).
* Install your menu into your parent component inside of an element with a class or id as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditor/Component.purs#L74-L80).

### Dismissing submenus when `onclick` events are triggered outside of a menu component
* Import `Halogen.Menu.Query (MenuQuery(..))`.
* Define a non propogating and default preventing action which is triggered by the onclick event of the root element of your application as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditor/Component.purs#L71).
* Define the evaluation of this action such that it queries your menu component with the `DismissSubmenu` action as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditor/Component.purs#L93-L95).

### Making a menu perform actions
* Import `Halogen.Menu.Submenu.Query (SubmenuQuery(..))`.
* Define a peek function for your parent component which transforms the `SelectSubmenuItem` query containing the values you have specified into the operations you wish to perform. In [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/src/ColorEditor/Component.purs#L97-L104) this is routing a query to another component.

### Styling a menu
* Use the [included template](stylesheet.css) to define styling for the `ul`s, `li`s and `button`s in your menu using the class or id you defined previously as a root element as in [this example](https://github.com/beckyconning/color-editor/tree/94de4b0297ef1ed6e76561ec88234be0ca7f07bd/stylesheet.css#L17-L72).

## Module documentation

Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-halogen-menu).

## Example
An example Halogen application which uses `purescript-halogen-menu` is available [here](https://github.com/beckyconning/color-editor).
