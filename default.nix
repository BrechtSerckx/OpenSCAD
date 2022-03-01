let
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  cleanSource = pkgs.haskell-nix.haskellLib.cleanGit;
in pkgs.haskell-nix.project {
  src = cleanSource {
    name = "OpenSCAD";
    src = ./.;
  };
  # materialized = ./OpenSCAD.materialized;
}
