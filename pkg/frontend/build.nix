{ nixpkgs ? import ./nixpkgs
, compiler ? "ghcjsHEAD"
}:

let
  react-hs-repo = import ./react-hs;
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              react-hs = pkgs.haskell.lib.dontHaddock (
                haskellPackagesNew.callPackage "${react-hs-repo}/react-hs/default.nix" { }
              );

              refine-prelude = pkgs.haskell.lib.dontHaddock (
                haskellPackagesNew.callPackage ../prelude/default.nix { }
              );

              refine-common = pkgs.haskell.lib.dontHaddock (
                haskellPackagesNew.callPackage ../common/default.nix { inherit refine-prelude; }
              );

              refine-frontend = pkgs.haskell.lib.dontHaddock (
                haskellPackagesNew.callPackage ./default.nix { inherit react-hs refine-prelude refine-common; }
              );

            };
          };
        };
      };
    };
  };

  pkgs = nixpkgs { inherit config; };

in
  { refine-frontend = pkgs.haskell.packages.${compiler}.refine-frontend;
  }
