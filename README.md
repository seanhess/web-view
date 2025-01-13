Web View
============

[![Hackage](https://img.shields.io/hackage/v/web-view.svg)][hackage]

Type-safe HTML and CSS with intuitive layout and composable styles. Inspired by Tailwindcss and Elm-UI

### Write Haskell instead of CSS

Type-safe utility functions to generate styled HTML.

```haskell
myPage = col (gap 10) $ do
  el (bold . fontSize 32) "My page"
  button (border 1) "Click Me"
```

Leverage the full power of Haskell functions for reuse, instead of relying on CSS.

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
    col (section . grow) "Main Content"
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

### Try Example Project with Nix

If you want to get a feel for web-view without cloning the project run `nix run github:seanhess/web-view` to run the example webserver locally

Local Development
-----------------

### Recommended ghcid command

If you want to work on both the web-view library and example code, this `ghcid` command will run and reload the examples server as you change any non-testing code.

```
ghcid --command="cabal repl exe:example lib:web-view" --run=Main.main --warnings --reload=./embed/preflight.css
```

If you want to work on the test suite, this will run the tests each time any library code is changed.

```
ghcid --command="cabal repl test lib:web-view" --run=Main.main --warnings --reload=./embed/preflight.css
```

### Nix

Prepend targets with ghc982 or ghc966 to use GHC 9.8.2 or GHC 9.6.6

- `nix run` or `nix run .#ghc966-example` to start the example project with GHC 9.8.2
- `nix develop` or `nix develop .#ghc982-shell` to get a shell with all dependencies installed for GHC 9.8.2. 

You can import this flake's overlay to add `web-view` to all package sets and override ghc966 and ghc982 with the packages to satisfy `web-view`'s dependencies.

```nix
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    web-view.url = "github:seanhess/web-view"; # or "path:/path/to/cloned/web-view";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, web-view, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ web-view.overlays.default ];
        };
        haskellPackagesOverride = pkgs.haskell.packages.ghc966.override (old: {
          overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (hfinal: hprev: {
            # your overrides here
          });
        });
      in
      {
        devShells.default = haskellPackagesOverride.shellFor {
          packages = p: [ p.web-view ];
        };
      }
    );
}
```

Learn More
----------

View Documentation on [Hackage][hackage]
* https://hackage.haskell.org/package/web-view

View on Github
* https://github.com/seanhess/web-view

View [Examples](https://github.com/seanhess/web-view/blob/latest/example/app/Main.hs)


[hackage]: https://hackage.haskell.org/package/web-view


Contributors
------------

* [Sean Hess](https://github.com/seanhess)
* [Kamil Figiela](https://github.com/kfigiela)
* [Pfalzgraf Martin](https://github.com/Skyfold)

