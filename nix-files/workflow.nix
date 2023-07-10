{ workflows, system, name, scripts }:
let
  inherit (workflows.lib.${system})
    writeWorkflow expr mkAccessors genAttrsId
    steps run os nixCI;
  job1 = "_1_update_flake_locks";
  job2 = "_2_front";
  names = mkAccessors {
    secrets = genAttrsId [ "GITHUB_TOKEN" ];
  };
  workflow =
    nixCI { } // {
      jobs = {
        "${job1}" = (nixCI { cacheNixArgs = { linuxMaxStoreSize = 15000000000; macosMaxStoreSize = 15000000000; }; }).jobs.nixCI;
        "${job2}" =
          {
            name = "Publish static files";
            permissions = {
              contents = "write";
            };
            runs-on = os.ubuntu-20;
            steps = [
              steps.checkout
              (steps.installNix { })
              {
                name = "Build docs";
                run = run.nixScript { name = scripts.genDocs.pname; };
              }
              {
                name = "GitHub Pages action";
                uses = "peaceiris/actions-gh-pages@v3.9.3";
                "with" = {
                  github_token = expr names.secrets.GITHUB_TOKEN;
                  publish_dir = "./docs/dist";
                  force_orphan = true;
                };
              }
            ];
          };
      };
    };
in
writeWorkflow name workflow
