## Module Halogen.Menu.Submenu.Component.State

#### `Submenu`

``` purescript
type Submenu a = Array (SubmenuItem a)
```

#### `SubmenuItem`

``` purescript
type SubmenuItem a = { label :: String, shortcutLabel :: Maybe String, value :: a }
```

