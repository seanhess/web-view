{
  description = "web-view overlay, development and examples";

  inputs = {
    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nix-filter.url = "github:numtide/nix-filter/main";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    inputs@{
      self,
      flake-utils,
      nix-filter,
      git-hooks,
      ...
    }:
    let
      packageName = "web-view";
      src = nix-filter.lib {
        root = ./.;
        include = [
          (nix-filter.lib.inDirectory "src")
          (nix-filter.lib.inDirectory "embed")
          (nix-filter.lib.inDirectory "test")
          ./README.md
          ./CHANGELOG.md
          ./LICENSE
          ./${packageName}.cabal
          ./cabal.project
          ./package.yaml
          ./fourmolu.yaml
        ];
      };

      overlay = final: prev: {
        # see https://github.com/NixOS/nixpkgs/issues/83098
        cabal2nix-unwrapped = prev.haskell.lib.justStaticExecutables prev.haskell.packages.ghc94.cabal2nix;
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions prev.haskell.packageOverrides (
            hfinal: hprev: {
              "${packageName}" = hfinal.callCabal2nix packageName src { };
            }
          );
          packages = prev.haskell.packages // {
            ghc982 = prev.haskell.packages.ghc982.override (old: {
              overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
                hfinal: hprev: {
                  skeletest = hprev.skeletest.overrideAttrs (old: {
                    meta = old.meta // {
                      broken = false;
                    };
                  });
                  Diff = hfinal.callHackage "Diff" "0.5" { };
                }
              );
            });
            ghc966 = prev.haskell.packages.ghc966.override (old: {
              overrides = prev.lib.composeExtensions (old.overrides or (_: _: { })) (
                hfinal: hprev: {
                  attoparsec-aeson = hfinal.callHackage "attoparsec-aeson" "2.2.0.0" { };
                  skeletest = hprev.skeletest.overrideAttrs (old: {
                    meta = old.meta // {
                      broken = false;
                    };
                  });
                  Diff = hfinal.callHackage "Diff" "0.5" { };
                  aeson = hfinal.callHackage "aeson" "2.2.2.0" { };
                }
              );
            });
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
        };

        ghcPkgs =
          (import inputs.nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
          }).haskell.packages;

        # Define GHC versions list
        ghcVersions = [
          "966"
          "982"
        ];

        example-src = nix-filter.lib {
          root = ./example;
          include = [
            (nix-filter.lib.inDirectory "app")
            ./example/example.cabal
            ./example/cabal.project
            ./example/LICENSE
          ];
        };

        pre-commit = git-hooks.lib.${system}.run {
          src = src;
          hooks = {
            hlint.enable = true;
            fourmolu.enable = true;
            hpack.enable = true;
            nixfmt-rfc-style.enable = true;
            flake-checker = {
              enable = true;
              args = [ "--no-telemetry" ];
            };
            check-merge-conflicts.enable = true;
          };
        };

        shellCommon = version: {
          inherit (pre-commit) shellHook;
          buildInputs = with pkgs.haskell.packages."ghc${version}"; [
            cabal-install
            haskell-language-server
            fast-tags
            ghcid
            fourmolu
            pkgs.hpack
          ];
          withHoogle = true;
          doBenchmark = true;
          CABAL_CONFIG = "/dev/null";
        };

        # Create examples for each GHC version
        examples = builtins.listToAttrs (
          map (version: {
            name = "ghc${version}-example";
            value = ghcPkgs."ghc${version}".callCabal2nix "example" example-src { };
          }) ghcVersions
        );

        examples-exe =
          version: pkgs.haskell.lib.justStaticExecutables self.packages.${system}."ghc${version}-example";
      in
      {
        checks = builtins.listToAttrs (
          builtins.concatMap (version: [
            {
              name = "ghc${version}-check-${packageName}";
              value = pkgs.runCommand "ghc${version}-check-${packageName}" {
                buildInputs = [ self.packages.${system}."ghc${version}-${packageName}" ];
              } "touch $out";
            }
            {
              name = "ghc${version}-check-example";
              value = pkgs.runCommand "ghc${version}-check-example" {
                buildInputs = [ (examples-exe version) ];
              } "type example; touch $out";
            }
          ]) ghcVersions
        );

        apps =
          {
            default = self.apps.${system}.ghc966-example;
          }
          // builtins.listToAttrs (
            # Generate apps
            map (version: {
              name = "ghc${version}-example";
              value = {
                type = "app";
                program = "${examples-exe version}/bin/example";
              };
            }) ghcVersions
          );

        packages =
          {
            default = self.packages.${system}."ghc982-${packageName}";
          }
          // builtins.listToAttrs (
            # Generate packages
            builtins.concatMap (version: [
              {
                name = "ghc${version}-${packageName}";
                value = ghcPkgs."ghc${version}".${packageName};
              }
              {
                name = "ghc${version}-example";
                value = examples."ghc${version}-example";
              }
            ]) ghcVersions
          );

        devShells =
          {
            default = self.devShells.${system}."ghc982-${packageName}";
          }
          // builtins.listToAttrs (
            # Generate devShells
            builtins.concatMap (version: [
              {
                name = "ghc${version}-${packageName}";
                value = ghcPkgs."ghc${version}".shellFor (
                  shellCommon version // { packages = p: [ p.${packageName} ]; }
                );
              }
              {
                name = "ghc${version}-example";
                value = ghcPkgs."ghc${version}".shellFor (
                  shellCommon version // { packages = _: [ examples."ghc${version}-example" ]; }
                );
              }
            ]) ghcVersions
          );
      }
    );
}
