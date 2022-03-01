let
  project = import ./default.nix;
  sources = import ./nix/sources.nix { };
  haskellNix = import sources.haskellNix { };
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in project.shellFor {
  packages = ps: with ps; [ OpenSCAD ];

  withHoogle = true;

  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.8";
    ghcid = "0.8.7";
  };

  buildInputs = with (import sources.nixpkgs { }); [ nixfmt ];

  crossPlatforms = ps:
    with ps;
    [
      # ghcjs      
      # mingwW64 
    ];

  exactDeps = true;
}
