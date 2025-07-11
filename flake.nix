{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-25.05-darwin";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { localSystem = { inherit system; }; };
    in
    {
      devShells.default = pkgs.mkShell ({
        buildInputs = [
			# Haskell
			pkgs.haskell.compiler.ghc910
			pkgs.cabal-install
			(pkgs.haskell-language-server.override { supportedGhcVersions = [ "910" ]; supportedFormatters = [ "fourmolu" ]; })
			pkgs.haskellPackages.cabal-fmt
			pkgs.haskellPackages.fourmolu
			pkgs.haskellPackages.hlint
			pkgs.haskellPackages.ghcprofview
        ];

        shellHook = ''
        '';
      });
    }
  );
}
    
