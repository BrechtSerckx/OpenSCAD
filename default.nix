{ packageSet ? null }:
let
  pkgs = import ./nix/pkgs.nix;
  cleanSource = pkgs.haskell-nix.haskellLib.cleanGit;
in pkgs.haskell-nix.stackProject {
  stackYaml = if isNull packageSet || packageSet == "lts-20" then
    "stack.yaml"
  else if builtins.elem packageSet [ "lts-19" "lts-18" ] then
    "stack-${toString packageSet}.yaml"
  else
    abort "packageSet must be one of <null>, lts-19 or lts-18";
  src = cleanSource {
    name = "OpenSCAD";
    src = ./.;
  };
  modules =
    [{ packages.OpenSCAD.components.library.ghcOptions = [ "-Werror" ]; }];
}
