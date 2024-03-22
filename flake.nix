{
  description = "Haskell Flake";
  inputs = {
    nixpkgs = {url = "github:NixOS/nixpkgs/nixos-23.05";};
    flake-utils = {url = "github:numtide/flake-utils";};
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      shellHook = "$SHELL";
    in
      with pkgs; {
        devShell = haskellPackages.shellFor {
          packages = p: [
          ];

          shellHook = shellHook;
          
          buildInputs = with haskellPackages; [
            cabal-install
          ];
        };
      });
}
