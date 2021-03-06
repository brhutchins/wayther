{ nixpkgs ? import <nixpkgs> {} }:

let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = haskellPackages.callPackage ./default.nix {};
in

with pkgs;

mkShell {
  name = "wayther";
  buildInputs = project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
    haskell-language-server
    hpack
  ];
}
