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
        cacheNixArgs = {
          linuxGCEnabled = true;
          linuxMaxStoreSize = 15000000000;
          macosGCEnabled = true;
          macosMaxStoreSize = 15000000000;
        };
        steps = _: stepsIf "${names.matrix.os} == '${os.ubuntu-22}'" [
          {
            name = "Build docs";
            run = ''
              ${run.nixScript { name = scripts.genDocs.pname; }}
              cp -r docs/book docs/dist
            '';
          }
          {
            name = "Commit & Push docs";
            run = ''
              git add "docs/src"
              git commit -m "Update docs" && git push || echo "push failed!"
            '';
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
in
writeWorkflow name workflow
