{
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      myPkg =
        pkgs: returnShellEnv:
        pkgs.haskellPackages.developPackage {
          root = ./.;
          inherit returnShellEnv;
        };
    in
    {
      devShells.${system}.default = pkgs.mkShellNoCC {
        inputsFrom = [ (myPkg pkgs true) ];
        packages = with pkgs; [
          nixfmt-rfc-style
          # TODO should we put haskell-related tools into myPkgs to ensure version compatibility
          # check how haskell-flake (by srid) does this
          cabal-install
          haskell-language-server
        ];
      };
      packages.${system} = {
        default = pkgs.haskell.lib.compose.justStaticExecutables (myPkg pkgs false);
        full = myPkg pkgs false;
      };
      overlays = {
        default = final: prev: {
          mail-notifier = final.haskell.lib.compose.justStaticExecutables (myPkg prev false);
        };
      };
    };
}
