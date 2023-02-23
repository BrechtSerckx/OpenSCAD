let
  pkgs = import ./nix/pkgs.nix;
  cleanSource = pkgs.haskell-nix.haskellLib.cleanGit;
in pkgs.haskell-nix.project {
  src = cleanSource {
    name = "OpenSCAD";
    src = ./.;
  };
  # materialized = ./OpenSCAD.materialized;
}
