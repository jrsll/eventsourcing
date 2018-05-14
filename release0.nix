let
    bootstrap = import <nixpkgs> { };

    pkgsInfo = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

    pkgsSrc = bootstrap.fetchFromGitHub {
        owner = "NixOS";
        repo  = "nixpkgs";
        inherit (pkgsInfo) rev sha256;
    };

    pkgs = import pkgsSrc { };

in

{ compiler   ? "default"
, withHoogle ? false
}:

let

  haskellPackagesBaseSet =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};

  haskellPackagesSet =
    if withHoogle then
      haskellPackagesBaseSet.override {
        overrides = (self: super: {
          ghc             = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        });
      }
     else
        haskellPackagesBaseSet;
          
in

    haskellPackagesBaseSet.callPackage ./default.nix { }
