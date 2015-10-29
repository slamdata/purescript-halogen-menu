## Module Halogen.Menu.Model

#### `Menu`

``` purescript
type Menu a = { chosen :: Maybe Int, submenus :: Array (MenuItem a) }
```

#### `MenuItem`

``` purescript
type MenuItem a = { label :: String, submenu :: Submenu a }
```

#### `makeMenu`

``` purescript
makeMenu :: forall a. Array (MenuItem a) -> Menu a
```


