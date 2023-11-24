Web View
============

Type-safe HTML and CSS with simplified layout and easy composition of styles. Inspired by Tailwindcss and Elm-UI

[![Hackage](https://img.shields.io/hackage/v/web-view.svg)][hackage]

Write Haskell instead of CSS
----------------------------

This library provides type-safe utility functions to style html

    myPage = col (gap 10) $ do
      el (bold . fontSize 32) "My page"
      button (color Red) "Click Me"

Rather than coming up with names for CSS classes to re-use styles, you can use haskell functions

    header = bold
    h1 = header . fontSize 32
    h2 = header . fontSize 24
    page = gap 10

    myPage = col page $ do
      el h1 "My Page"
      ...

This approach is inspired by Tailwindcss' [Utility Classes](https://tailwindcss.com/docs/utility-first)

Simplified Layouts
------------------

Easily create layouts with `row`, `col`, and `space`. See [Example.Layout](blob/main/example/Example/Layout.hs)

https://github.com/seanhess/web-view/blob/52fafd9620f2df88197733a436c1af12b3533d88/example/Example/Layout.hs#L36-L48


Embedded CSS
------------

Views track which styles are used in any child node, and automatically embed all CSS when rendered. 

    >>> renderText () $ el bold "Hello"
    <style type='text/css'>.bold { font-weight:bold }</style>
    <div class='bold'>Hello</div>


State variants
--------------

We can apply styles when certain states apply. For example, to change the background on hover:

    button (bg Primary . hover (bg PrimaryLight)) "Hover Me"

Media states allow us to create responsive designs

    el (width 100 . media (MinWidth 800) (width 400))
      "Big if window > 800"


Learn More
----------

View Documentation on [Hackage][hackage]
* https://hackage.haskell.org/package/aeson-2.2.1.0

View on Github
* https://github.com/seanhess/web-view




#### Why function composition instead of lists?

It makes dynamic styles cleaner, without any need to concat anything

    view user = el (selected . bold . fontSize 32) (text user.name)
      where selected = if isSelected then color Red else id

[hackage]: https://hackage.haskell.org/package/aeson-2.2.1.0
