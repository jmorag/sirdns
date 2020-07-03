let
  default_nixpkgs = (import <nixpkgs> { }).fetchFromGitHub {
    name = "nixos-unstable-2020-05-29";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "19aac2413ae2810a47850f165a592cbe0d06f744";
    sha256 = "0m0h02avy888zmkrys9azs6sqx10my3gxi3f0m70fhsyc6shpc78";
  };

in { nixpkgs ? default_nixpkgs, compiler ? "ghc883" }:
let
  hsOverlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" = super.haskell.packages."${compiler}".override {
          overrides = haskellSelf: haskellSuper: {
            ghc = haskellSuper.ghc // {
              withPackages = haskellSuper.ghc.withHoogle;
            };
            ghcWithPackages = haskellSelf.ghc.withPackages;
          };
        };
      };
    };
  };

  orig_pkgs = import nixpkgs { };
  pkgs = import orig_pkgs.path { overlays = [ hsOverlay ]; };
  ghc = pkgs.haskell.packages."${compiler}";
  pkg = ghc.developPackage {
    root = ./.;
    source-overrides = { };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv
      (with pkgs.haskellPackages; [ cabal-install ghcid ghcide hlint hoogle ]);
  };
  buildInputs = [ pkgs.wireshark pkgs.dnsutils ];

in pkg.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ buildInputs; })
