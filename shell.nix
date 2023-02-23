let
  project = import ./default.nix;
  pkgs = import ./nix/pkgs.nix;
  sources = import ./nix/sources.nix { };
in project.shellFor {
  packages = ps: with ps; [ OpenSCAD ];

  withHoogle = true;

  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.8";
    ghcid = "0.8.7";
    ormolu = "0.1.4.1";
  };

  buildInputs = with pkgs; [
    nixfmt
    (import sources.nixpkgs-act { }).act
  ];

  exactDeps = true;
}
