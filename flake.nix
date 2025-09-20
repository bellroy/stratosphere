{
  inputs = {
    bellroy-nix-foss.url = "github:bellroy/bellroy-nix-foss";
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs =
    inputs:
    inputs.bellroy-nix-foss.lib.haskellProject {
      extraTools = _: ([
        inputs.nixpkgs.legacyPackages.aarch64-darwin.stack
      ]);
      src = ./.;
      supportedCompilers = [
        "ghc98"
      ];
      defaultCompiler = "ghc98";
    };
}
