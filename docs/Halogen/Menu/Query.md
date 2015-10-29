## Module Halogen.Menu.Query

#### `MenuQuery`

``` purescript
data MenuQuery a next
  = SelectSubmenu Int next
  | DismissSubmenu next
```

Query algebra for menu components.


