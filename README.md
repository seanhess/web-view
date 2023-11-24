Web View
============

Type-safe HTML and CSS with intuitive layout and composable styles. Inspired by Tailwindcss and Elm-UI

[![Hackage](https://img.shields.io/hackage/v/web-view.svg)][hackage]

### Write Haskell instead of CSS

Type-safe utility functions to style html

```haskell
myPage = col (gap 10) $ do
  el (bold . fontSize 32) "My page"
  button (border 1) "Click Me"
```

Re-use styles as Haskell functions instead of naming CSS classes.

```haskell
header = bold
h1 = header . fontSize 32
h2 = header . fontSize 24
page = gap 10

myPage = col page $ do
  el h1 "My Page"
  ...
```

This approach is inspired by Tailwindcss' [Utility Classes](https://tailwindcss.com/docs/utility-first)

### Intuitive Layouts

Easily create layouts with `row`, `col`, `grow`, and `space`

```haskell
holygrail :: View c ()
holygrail = layout id $ do
  row section "Top Bar"
  row grow $ do
    col section "Left Sidebar"
    col section "Main Content"
    col section "Right Sidebar"
  row section "Bottom Bar"
  where section = 'border' 1
```

### Embedded CSS

Views track which styles are used in any child node, and automatically embed all CSS when rendered. 

    >>> renderText $ el bold "Hello"
    
    <style type='text/css'>.bold { font-weight:bold }</style>
    <div class='bold'>Hello</div>


### Stateful Styles

We can apply styles when certain states apply. For example, to change the background on hover:

```haskell
button (bg Primary . hover (bg PrimaryLight)) "Hover Me"
```

Media states allow us to create responsive designs

```haskell
el (width 100 . media (MinWidth 800) (width 400))
  "Big if window > 800"
```


Learn More
----------

View Documentation on [Hackage][hackage]
* https://hackage.haskell.org/package/web-view-0.2.0

View on Github
* https://github.com/seanhess/web-view

View [Examples](https://github.com/seanhess/web-view/blob/main/example/Main.hs)


[hackage]: https://hackage.haskell.org/package/web-view-0.2.0
