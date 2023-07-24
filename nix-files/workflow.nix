{ workflows, system, name, scripts }:
let
  inherit (workflows.lib.${system})
    writeWorkflow expr mkAccessors genAttrsId
    steps run os nixCI names stepsIf;
  job1 = "_1_update_flake_locks";
  job2 = "_2_front";
  workflow =
    nixCI
      {
        jobArgs = {
          cacheNixArgs = {
            linuxGCEnabled = true;
            linuxMaxStoreSize = 15000000000;
            macosGCEnabled = true;
            macosMaxStoreSize = 15000000000;
          };
          doCommit = false;
          steps = { stepsAttrs, ... }:
            stepsIf "${names.matrix.os} == '${os.ubuntu-22}'" [
              {
                name = "Update docs";
                run = run.nixScript { name = scripts.genDocs.pname; };
              }
              (steps.commit { messages = [ (steps.updateLocks { }).name (steps.format { }).name stepsAttrs."Update docs".name ]; })
              {
                name = "Copy docs";
                run = "cp -r docs/book docs/dist";
              }
              {
                name = "Publish docs to GitHub Pages";
                uses = "peaceiris/actions-gh-pages@v3.9.3";
                "with" = {
                  github_token = expr names.secrets.GITHUB_TOKEN;
                  publish_dir = "docs/dist";
                  force_orphan = true;
                };
              }
            ];
        };
      };
in
writeWorkflow name workflow
