let
  project = import ./default.nix;
  pkgs = import ./nix/pkgs.nix;
  sources = import ./nix/sources.nix { };
in project.shellFor {
  packages = ps: with ps; [ OpenSCAD ];

  withHoogle = true;

  tools = {
    cabal = "3.6.2.0";
    hlint = "3.5";
    ghcid = "0.8.7";
    ormolu = "0.5.0.1";
    haskell-ci = "0.14.3";
    haskell-language-server = "1.9.1.0";
    cabal-fmt = "0.1.6";
  };

  buildInputs = with pkgs; [
    nixfmt
    (import sources.nixpkgs-act { }).act
    cmake
    bash
    openscad
  ];

  exactDeps = true;
}
