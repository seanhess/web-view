{
  description = "web-view";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter/main";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{
      self,
      flake-utils,
      nix-filter,
      ...
    }:
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

      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev: {
            web-view = hfinal.callCabal2nix "web-view" web-view-src { };
          };
        };
      };
    in
    {
      overlays.default = overlay;
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
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

        web-view-haskellPackages = pkgs.haskellPackages.extend (
          hfinal: hprev: {
            attoparsec-aeson = hfinal.callHackage "attoparsec-aeson" "2.2.0.0" { };
            skeletest = hfinal.callHackage "skeletest" "0.1.0" { };
            Diff = hfinal.callHackage "Diff" "0.5" { };
            aeson = hfinal.callHackage "aeson" "2.2.2.0" { };
          }
        );

        shellCommon = {
          # don't use the modified package set to build dev tools
          buildInputs = with pkgs; [
            haskellPackages.cabal-install
            haskell-language-server
            haskellPackages.fast-tags
            haskellPackages.ghcid
          ];
          withHoogle = true;
          doBenchmark = true;
        };

        selfPkgs = self.packages.${system};

      in
      {
        packages = {
          default = selfPkgs.web-view-haskellPackages.web-view;
          web-view = selfPkgs.default;
          web-view-haskellPackages = web-view-haskellPackages;
        };
        inherit pkgs;

        devShells = {
          default = self.devShells.${system}.web-view;
          web-view = selfPkgs.web-view-haskellPackages.shellFor (
            shellCommon // { packages = p: [ p.web-view ]; }
          );
          example = selfPkgs.web-view-haskellPackages.shellFor (
            shellCommon
            // {
              packages = _: [
                (selfPkgs.web-view-haskellPackages.callCabal2nix "example" example-src { })
              ];
            }
          );
        };
      }
    );
}
