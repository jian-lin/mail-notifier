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
              HaskellNet =
                hprev.HaskellNet
                |> hlib.markUnbroken
                |> hlib.overrideSrc {
                  # use a fork of wireapp to easily apply https://github.com/qnikst/HaskellNet/pull/98
                  src = final.fetchFromGitHub {
                    owner = "wireapp";
                    repo = "HaskellNet";
                    rev = "74cde03b4beb09794a6120ea5321a09430bcd2c7";
                    hash = "sha256-VIM60sXCVC25ULf/2yPvqANK/h9BY6dEYY3o3/xiEEQ=";
                  };
                  # 0.6.1.2 is the latest version on Hackage
                  # even though the latest tag of upstream source repo is 0.6.1.0
                  version = "0.6.1.2-unstable-2025-05-08";
                };
            }
          );
        });
      };
      packages.${system} = {
        default = pkgs.lib.getBin self.packages.${system}.all;
        all = haskellPackages.generateOptparseApplicativeCompletions [
          "mail-notifier"
        ] (pkgs.haskell.lib.compose.enableSeparateBinOutput haskellPackages.${projectName});
      };
      devShells.${system}.default = haskellPackages.shellFor {
        packages = hpkgs: [ hpkgs.${projectName} ];
        nativeBuildInputs =
          (with haskellPackages; [
            cabal-install
            cabal-fmt
            ghcid
            haskell-language-server
            ormolu # TODO replace it with treefmt?
          ])
          ++ (with pkgs; [
            nixfmt-rfc-style
          ]);
        withHoogle = true;
        doBenchmark = true;
      };
    };
}
