args@{ packageSet ? null }:
let
  project = import ./default.nix args;
  pkgs = import ./nix/pkgs.nix;
  sources = import ./nix/sources.nix { };
in project.shellFor {
  packages = ps: with ps; [ OpenSCAD ];

  withHoogle = true;

  tools = if isNull packageSet || packageSet == "lts-20" then {
    cabal = "3.6.2.0";
    hlint = "3.5";
    ghcid = "0.8.7";
    ormolu = "0.5.0.1";
    haskell-ci = "0.14.3";
    haskell-language-server = "1.9.1.0";
    cabal-fmt = "0.1.6";
  } else if packageSet == "lts-19" then {
    cabal = "3.4.1.0";
    ghcid = "0.8.7";
    haskell-language-server = "1.9.0.0";
  } else if packageSet == "lts-18" then {
    cabal = "3.2.0.0";
    ghcid = "0.8.7";
    haskell-language-server = "1.8.0.0";
  } else
    abort "packageSet must be one of <null>, lts-19 or lts-18";

  buildInputs = with pkgs; [
    nixfmt
    (import sources.nixpkgs-act { }).act
    cmake
    bash
    openscad
    # not available from haskell.nix?
    pkgs.haskellPackages.doctest
  ];

  exactDeps = true;

  shellHook = if isNull packageSet then
    ""
  else ''
    export PACKAGE_SET=${packageSet}
  '';
}
