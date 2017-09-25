let
  pkgs = import ./nixpkgs { };
  pkg = path: deps: pkgs.haskell.lib.dontHaddock ( pkgs.haskell.packages.ghcjsHEAD.callPackage path deps );

  react-hs        = pkg "${import ./react-hs}/react-hs/default.nix" { };
  refine-prelude  = pkg ../prelude/default.nix { };
  refine-common   = pkg ../common/default.nix { inherit refine-prelude; };
  refine-frontend = pkg ./default.nix { inherit refine-prelude refine-common react-hs; };

in refine-frontend

