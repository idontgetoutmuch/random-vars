let

myHaskellPackageOverlay = self: super: {

  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      random = self.haskell.lib.addBuildDepends (super.haskell.lib.dontHaddock (super.haskell.lib.dontCheck (
        hself.callCabal2nixWithOptions "random" (builtins.fetchGit {
          url = "https://github.com/haskell/random";
          rev = "edae4f7908f3c7e00be1094034a4a09cd72ab35e";
        }) "" { }
      ))) [ ];

      hashable = super.haskell.lib.doJailbreak hsuper.hashable;

    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { overlays = [ myHaskellPackageOverlay ]; }, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, erf, free, lib, mtl, random, transformers, vector }:
      mkDerivation {
        pname = "monad-bayes";
        version = "0.0.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [ base bytestring erf free mtl random transformers vector ];
        homepage = "http://github.com/idontgetoutmuch/random-vars";
        description = "Random variables";
        license = lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.myHaskellPackages
                       else pkgs.myHaskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
