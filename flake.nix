{
  description = "web-view";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter/main";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, flake-utils, nix-filter, ... }: 
    let
      web-view-src = nix-filter.lib {
        root = ./.;
        include = [
          (nix-filter.lib.inDirectory "src")
          (nix-filter.lib.inDirectory "embed")
          (nix-filter.lib.inDirectory "test")
          ./README.md
          ./CHANGELOG.md
          ./LICENSE
          ./web-view.cabal
        ];
      };

      # build cabal2nix with a different package set as suggested by https://github.com/NixOS/nixpkgs/issues/83098#issuecomment-602132784
      # Only with overlay, cabal2nix causes infinite recursion if one of it's dependencies is overridden
      fixCabal2nix = self: super: {
        cabal2nix-unwrapped = super.haskell.lib.justStaticExecutables
          super.haskell.packages."ghc902".cabal2nix;
      };
      haskellOverlay = final: prev: {
        haskellPackages = prev.haskellPackages.override {
          overrides = hself: hsuper: {
            web-view = hself.callCabal2nix "web-view" web-view-src { };
            attoparsec-aeson = hself.callHackage "attoparsec-aeson" "2.2.0.0" {};
            skeletest = hself.callHackage "skeletest" "0.1.0" {};
            Diff = hself.callHackage "Diff" "0.5" {};
            aeson = hself.callHackage "aeson" "2.2.2.0" {};
          } // (final.lib.optionalAttrs (final.system == "x86_64-darwin" || final.system == "aarch64-darwin") {
            crypton = final.haskell.lib.dontCheck hsuper.crypton;
          });
        };
      };
      overlay = final: prev:
        let
          fApplied = fixCabal2nix final prev;
          prev' = prev // fApplied;
        in
        fApplied // haskellOverlay final prev';
    in
      flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            (nix-filter.lib.inDirectory "app")
            ./example/example.cabal
            ./example/cabal.project
          ];
        };

        shellCommon = {
          # don't use the modified package set to build dev tools
          buildInputs = with inputs.nixpkgs.legacyPackages.${system}; [
            haskellPackages.cabal-install
            haskell-language-server
            haskellPackages.fast-tags
            haskellPackages.ghcid
          ];
          withHoogle = true;
          doBenchmark = true;
        };

      in
      {
        overlays.default = overlay;
        packages = {
          default = pkgs.haskellPackages.web-view;
          web-view = pkgs.haskellPackages.web-view;
        };

        devShells = {
          default = self.devShells.${system}.web-view;
          web-view = pkgs.haskellPackages.shellFor (shellCommon // {
            packages = p: [ p.web-view ];
          });
          example = pkgs.haskellPackages.shellFor (shellCommon // {
            packages = _: [
              (pkgs.haskellPackages.callCabal2nix "example" example-src {})
            ];
          });
        };
      }
    );
}
