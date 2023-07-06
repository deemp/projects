{ drv-tools, system, pkgs }:
let
  inherit (drv-tools.lib.${system}) writeYAML mkShellApp mkBin;
  inherit (pkgs.lib.attrsets) mapAttrsToList recursiveUpdate;
  inherit (pkgs.lib.lists) flatten;
  inherit (builtins) foldl';

  writeConfigs =
    let
      config = import ./config.nix;
      contactsConfig =
        foldl' (a: b: recursiveUpdate a b) { }
          (flatten
            (mapAttrsToList
              (name: value:
                mapAttrsToList
                  (name_: value_:
                    let val = {
                      file = "${config.dataDir}/${config.contacts.dir}/${value_.file}.yaml";
                      starts =
                        # in config {a.b = {}}, the root is "a"
                        if value_.startsRoot
                        then name
                        else name_;
                    }; in
                    {
                      "${name}"."${name_}" = val;
                      "${name_}"."${name}" = val;
                    })
                  value)
              config.contacts.config
            ));
      mkConfigPath = file: "${config.dataDir}/${config.configsDir}/${file}.yaml";
      writeConfig = file: config_: writeYAML file (mkConfigPath file) config_;
      mainConfig = {
        contacts = mkConfigPath config.contacts.file;
        server = mkConfigPath config.server.file;
        spawner = mkConfigPath config.spawner.file;
        contactsDir = "${config.dataDir}/${config.contacts.dir}";
        port = config.port;
      };
      writeContactsConfig = writeConfig config.contacts.file contactsConfig;
      writeServerConfig = writeConfig config.server.file config.server.config;
      writeSpawnerConfig = writeConfig config.spawner.file config.spawner.config;
      writeMainConfig = writeYAML "main" "${config.dataDir}/main.yaml" mainConfig;
    in
    mkShellApp {
      name = "write-configs";
      text = ''
        ${mkBin writeContactsConfig}
        ${mkBin writeServerConfig}
        ${mkBin writeSpawnerConfig}
        ${mkBin writeMainConfig}
      '';
    };
in
writeConfigs
