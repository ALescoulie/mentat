{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    systems.url = "github:nix-systems/default";
    ghc-wasm-meta.url = "https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz";
  };

  outputs = {
    systems,
    nixpkgs,
    ghc-wasm-meta,
    ...
  } @ inputs: let
    wasm = ghc-wasm-meta;
    eachSystem = f:
      nixpkgs.lib.genAttrs (import systems) (
        system:
          f nixpkgs.legacyPackages.${system}
          
      );
  in {
    devShells = eachSystem (pkgs: {
      default = pkgs.mkShell {
        buildInputs = [
          pkgs.haskellPackages.haskell-language-server
          pkgs.haskellPackages.hindent
          pkgs.haskell.compiler.ghc98
          pkgs.cabal-install
          pkgs.stack
          wasm.packages.${pkgs.system}.all_9_8
        ];
      };
    });
  };
}

