{
  outputs =
    { self, nixpkgs }:
    let
      projectName = "mail-notifier";

      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { };
        overlays = [ self.overlays.default ];
      };
      inherit (pkgs) haskellPackages;
    in
    {
      formatter.${system} = nixpkgs.legacyPackages.${system}.nixfmt-rfc-style;
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.override (args: {
          overrides = final.lib.composeExtensions args.overrides (
            let
              hlib = final.haskell.lib.compose;
            in
            hfinal: hprev: {
              ${projectName} =
                let
                  # filter out haskell-unrelated files to avoid unnecessary rebuilds
                  src = builtins.path {
                    path = ./.;
                    # NOTE setting name is important because the default one contains
                    # the store path of this flake, which defeats the motivation
                    name = "source";
                    filter =
                      path: type:
                      !builtins.elem (builtins.baseNameOf path) [
                        "flake.nix"
                        "flake.lock"
                        ".envrc"
                      ];
                  };
                in
                hfinal.callCabal2nix projectName src { };
              cabal-install = hlib.appendPatches [
                (final.fetchpatch {
                  name = "suppress-error-about-missing-local-index.patch";
                  url = "https://github.com/haskell/cabal/commit/d58a75ef4adab36688878420cc9e2c25bca41ec4.patch";
                  hash = "sha256-IZ+agNNN9AcIJBBsT30LAkAXCAoYKF+kIhccGPFdm+8=";
                  stripLen = 1;
                  includes = [ "src/Distribution/Client/IndexUtils.hs" ];
                })
              ] hprev.cabal-install;
            }
          );
        });
      };
      packages.${system}.default = haskellPackages.generateOptparseApplicativeCompletions [
        "mail-notifier"
      ] (pkgs.haskell.lib.compose.enableSeparateBinOutput haskellPackages.${projectName});
      devShells.${system}.default = haskellPackages.shellFor {
        packages = hpkgs: [ hpkgs.${projectName} ];
        nativeBuildInputs =
          (with haskellPackages; [
            cabal-install
            cabal-fmt
            ghcid
            haskell-language-server
          ])
          ++ (with pkgs; [
            nixfmt-rfc-style
          ]);
        withHoogle = true;
        doBenchmark = true;
      };
    };
}
