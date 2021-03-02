{
  description = "Cardano DB Sync";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, haskell-nix, ... }:
    (utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        legacyPackages = import ./nix {
          inherit system;
          ownHaskellNix = haskell-nix.legacyPackages.${system};
          gitrev = self.rev or "dirty";
        };

        lib = nixpkgs.lib;
        sources = import ./nix/sources.nix { };
        iohkNix = import sources.iohk-nix { inherit system; };
        environments = iohkNix.cardanoLib.environments;
        environmentName = "testnet";

        eachEnv = lib.flip lib.pipe [
          (lib.forEach (builtins.attrNames environments))
          lib.listToAttrs
        ];

        config = env:
          { pkgs, ... }: {
            services.cardano-db-sync = rec {
              enable = true;
              cluster = env;
              environment = environments.${env};
              logConfig = iohkNix.cardanoLib.defaultExplorerLogConfig;
              stateDir = "/persist";
              extended = true;
              package = pkgs.cardano-db-sync-extended;
              postgres = {
                generatePGPASS = true;
                generateDatabase = true;
                user = "cexplorer";
                database = "cexplorer";
                socketdir = "/alloc";
              };
            };

            services.postgresql = { enable = true; };
          };

        evaluated = env:
          lib.nixosSystem {
            inherit system;
            pkgs = legacyPackages;
            modules = [ ./nix/nixos/cardano-db-sync-service.nix (config env) ];
          };

        packages = (eachEnv (env:
          lib.nameValuePair "cardano-db-sync-extended-${env}" (let
            vanilla =
              legacyPackages.runCommand "cardano-db-sync-extended-entrypoint"
              { } ''
                install -Dm0777 ${
                  (evaluated env).config.services.cardano-db-sync.script
                } $out/bin/cardano-db-sync-extended-entrypoint
              '';
            deps = with legacyPackages; [
              coreutils
              findutils
              gnugrep
              gnused
              postgresql
              strace
              lsof
              dnsutils
              bashInteractive
              iproute
              curl
              netcat
              bat
              tree
            ];

            closure = legacyPackages.symlinkJoin {
              name = "cardano-db-sync-extended-entrypoint";
              paths = [ vanilla ] ++ deps;
            };
          in closure))) // {
            postgres = let
              deps = with legacyPackages; [
                coreutils
                findutils
                gnugrep
                gnused
                postgresql
                strace
                bashInteractive
              ];

              configs = rec {
                hba = legacyPackages.writeText "pg_hba.conf" ''
                  local all all trust
                '';
                ident = legacyPackages.writeText "pg_ident.conf" "";
                postgres = legacyPackages.writeText "postgresql.conf" ''
                  hba_file = '${hba}'
                  ident_file = '${ident}'
                  log_destination = 'stderr'
                  log_line_prefix = '[%p] '
                  unix_socket_directories = '/alloc'
                  listen_addresses = '''
                  max_locks_per_transaction = 1024
                  log_min_messages = 'DEBUG5'
                '';
              };

              PATH = lib.makeBinPath deps;

              entrypoint =
                legacyPackages.writeShellScriptBin "postgres-entrypoint" ''
                  set -exuo pipefail

                  export PATH="${PATH}"
                  export LOCALE_ARCHIVE=${legacyPackages.glibcLocales}/lib/locale/locale-archive

                  mkdir -p /run/postgresql
                  mkdir -p "$PGDATA"
                  chmod 0700 "$PGDATA"

                  if [ ! -s "$PGDATA/PG_VERSION" ]; then
                    initdb
                  fi

                  ln -sfn "${configs.postgres}" "$PGDATA/postgresql.conf"
                  exec postgres "$@"
                '';
            in legacyPackages.symlinkJoin {
              name = "postgres-entrypoint";
              paths = [ entrypoint ] ++ deps;
            };
          };
      in {
        inherit environments evaluated legacyPackages packages;

        apps = (eachEnv (env:
          lib.nameValuePair "cardano-db-sync-extended-${env}" (utils.lib.mkApp {
            drv = packages."cardano-db-sync-extended-${env}";
            exePath = "/bin/cardano-db-sync-extended-entrypoint";
          }))) // {
            postgres = utils.lib.mkApp {
              drv = packages.postgres;
              exePath = "/bin/postgres";
            };
          };
      }));
}
