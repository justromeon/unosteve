{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    process-compose-flake.url = "github:Platonic-Systems/process-compose-flake";
    services-flake.url = "github:juspay/services-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.process-compose-flake.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        {

          # Typically, you just want a single project named "default". But
          # multiple projects are also possible, each using different GHC version.
          haskellProjects.default = {

            autoWire = [
              "packages"
              "apps"
              "checks"
            ];

            # The base package set representing a specific GHC version.
            # By default, this is pkgs.haskellPackages.
            # You may also create your own. See https://haskell.nixos.asia/package-set
            # basePackages = pkgs.haskellPackages;

            # Extra package information. See https://haskell.nixos.asia/dependency
            #
            # Note that local packages are automatically included in `packages`
            # (defined by `defaults.packages` option).
            #
            packages = {
              # aeson.source = "1.5.0.0";      # Override aeson to a custom version from Hackage
              # shower.source = inputs.shower; # Override shower to a custom source path
            };
            settings = {
              #  aeson = {
              #    check = false;
              #  };
              #  relude = {
              #    haddock = false;
              #    broken = false;
              #  };
            };

            devShell = {
              # Enabled by default
              # enable = true;

              # Programs you want to make available in the shell.
              # Default programs can be disabled by setting to 'null'
              # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

              # Check that haskell-language-server works
              # hlsCheck.enable = true; # Requires sandbox to be disabled
            };
          };

          # `nix run .#dev`
          process-compose."dev" = {
            imports = [ inputs.services-flake.processComposeModules.default ];

            # Not secret: local Postgres uses trust auth with your OS user
            services.postgres."pg" = {
              enable = true;
              listen_addresses = "127.0.0.1";
              port = 5432;
              initialDatabases = [ { name = "unosteve"; } ];
            };

            # Apply migrations to the dev and test databases once Postgres is ready.
            # `dbmate up` also creates the database if it doesn't exist yet.
            settings.processes.migrate = {
              command = ''
                : "''${DATABASE_URL:?missing in .env}"
                : "''${TEST_DATABASE_URL:?missing in .env}"
                ${pkgs.dbmate}/bin/dbmate up
                ${pkgs.dbmate}/bin/dbmate --url "$TEST_DATABASE_URL" up
              '';
              depends_on."pg".condition = "process_healthy";
            };

          };

          devShells.default = pkgs.mkShell {
            name = "unosteve";
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
              config.process-compose."dev".services.outputs.devShell
            ];
            packages = [
              pkgs.awscli2
              pkgs.dbmate
            ];
          };

          # `nix fmt` formats everything; `nix flake check` fails on unformatted files
          treefmt = {
            projectRootFile = "flake.nix";
            programs.fourmolu.enable = true;
            programs.nixfmt.enable = true;
            programs.cabal-fmt.enable = true;
          };

          # `nix flake check` also runs hlint over all Haskell sources
          checks.hlint = pkgs.runCommand "hlint" { nativeBuildInputs = [ pkgs.hlint ]; } ''
            cd ${./.}
            hlint src app test
            touch $out
          '';

          # Building the package also runs the cabal test suite
          checks.unosteve = self'.packages.unosteve;

          # haskell-flake doesn't set the default package, but you can do it here.
          packages.default = self'.packages.unosteve;
        };
    };
}
