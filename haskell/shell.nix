let nixpkgs = import <nixpkgs> {};
    orig = nixpkgs.pkgs.haskellPackages.callPackage ./default.nix {};
in (nixpkgs.pkgs.haskell.lib.doBenchmark orig).env
