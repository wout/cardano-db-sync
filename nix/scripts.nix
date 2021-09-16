{ pkgs, customConfigs ? [ pkgs.customConfig ] }:
let
  inherit (pkgs) lib cardanoLib;
  inherit (pkgs.commonLib) evalService;
  blacklistedEnvs =
    [ "selfnode" "shelley_selfnode" "latency-tests" "mainnet-ci" ];
  environments = lib.filterAttrs (k: v: (!builtins.elem k blacklistedEnvs))
    cardanoLib.environments;
  mkScript = envConfig:
    let
      service = evalService {
        inherit pkgs customConfigs;
        serviceName = "cardano-db-sync";
        modules = customConfigs ++ [
          ./nixos/cardano-db-sync-service.nix
          {
            services.cardano-db-sync = {
              environment = lib.mkDefault envConfig;
              cluster = lib.mkDefault envConfig.name;
              dbSyncPkgs = lib.mkDefault pkgs;
              enable = true;
              stateDir = "/persist";
              extended = true;

              postgres = {
                generatePGPASS = true;
                generateDatabase = true;
                user = "cexplorer";
                database = "cexplorer";
                socketdir = "/alloc";
              };

              restoreSnapshot =
                "https://update-cardano-mainnet.iohk.io/cardano-db-sync/11/db-sync-snapshot-schema-11-block-6236059-x86_64.tgz";
              restoreSnapshotSha =
                "4be8a31a326467f2b946b5a851e56de86896e07babc03cfaa2467743c1b42676";
            };
          }
        ];
      };
    in lib.recurseIntoAttrs {
      db-sync = pkgs.writeScriptBin "cardano-db-sync-${service.cluster}" ''
        #!${pkgs.runtimeShell}
        set -euo pipefail
        ${service.script} $@
      '' // {
        passthru = { inherit service; };
      };
    };
in cardanoLib.forEnvironmentsCustom mkScript environments
