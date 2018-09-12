# default.nix
(import ./reflex-platform {}).project ({ pkgs, ... }: {

  overrides = self: super: {
   heist = pkgs.haskell.lib.doJailbreak super.heist;
   map-syntax = pkgs.haskell.lib.doJailbreak super.map-syntax;
   HList = pkgs.haskell.lib.doJailbreak super.HList;
  };
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    google-maps-reflex = ./google-maps-reflex;
    GoogleDirections = ./GoogleDirections;
    GoogleDistanceMatrix= ./GoogleDistanceMatrix;
  };

  shells = {
    ghc = ["common" "backend" "frontend"]; # "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };
})

