let
  x = import ./release0.nix { compiler = "ghc802"; withHoogle = true; };
in
  x.env

