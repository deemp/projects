# Tutorials and Notes

## Browser

1. You can add a search bookmark (Firefox) - [src](https://superuser.com/a/7336)

   - In a search box, click with the right mouse button
   - Click on `Add a keyword`

1. Mine are:
   - [nixman](https://nixos.org/manual/nix/unstable/command-ref/nix-store.html?searchbar=&search=s)
   - [stack](https://docs.haskellstack.org/en/stable/)
   - [devdoc](https://devdocs.io/nix/)
   - [nixpkgs](https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query=s&=)
   - [gh](https://github.com/search?type=&q=)
   - [docker](https://docs.docker.com/search/?q=s)
   - [halogen](https://purescript-halogen.github.io/purescript-halogen/index.html?search=s)

## Git

1. [git bisect](https://git-scm.com/docs/git-bisect#_basic_bisect_commands_start_bad_good) is your friend when searching for a problematic commit

1. When you want to contribute, [squash](https://htmlacademy.ru/blog/articles/how-to-squash-commits-and-why-it-is-needed) commits

   - It's easy with [Gitlens](https://marketplace.visualstudio.com/items?itemName=eamodio.gitlens):
     1. Find out how many new commits you have compared to `main`

        ```sh
        $ git cherry -v upstream/main | wc -l
        N
        ```

     1. Then rebase

        ```sh
        git rebase -i HEAD~N
        ```

     1. Choose to squash the latest commits

1. [git-subtree](https://www.atlassian.com/git/tutorials/git-subtree)
1. [git-filter-repo](https://github.com/newren/git-filter-repo)
1. Submodules: don't use them!
   - [Add](https://git-scm.com/book/en/v2/Git-Tools-Submodules) a submodule
   - Clone [nested submodules](https://stackoverflow.com/a/6562038)
   - Convert a submodule to a folder while preserving its history: [src](https://medium.com/walkme-engineering/how-to-merge-a-git-submodule-into-its-main-repository-d83a215a319c)

- `git rebase -Xtheirs another_branch` - to favor current branch over `another_branch` - [src](https://demisx.github.io/git/rebase/2015/07/02/git-rebase-keep-my-branch-changes.html)

## GitHub

1. GitHub dislikes `nix develop` and `nix-shel`. You should run commands via `nix develop -c bash -c 'command'`
1. Get info about forks: [gitpop3](https://andremiras.github.io/gitpop3/)

### Actions

1. You can use composite actions - [src](https://docs.github.com/en/actions/creating-actions/creating-a-composite-action)

   - Use composite-actions-specific [syntax](https://docs.github.com/en/actions/creating-actions/metadata-syntax-for-github-actions#runs-for-composite-actions)

1. Which variables are available to a composite action?

   - `env`, but [not](https://stackoverflow.com/a/70111134) `secrets`

1. Dynamically set env variable - [src](https://stackoverflow.com/a/70399393)

1. Develop a composite action

   - parameterize appropriately - [tutorial](https://colinsalmcorner.com/github-composite-actions/#case-study-eshoponcontainers)

1. GitHub permits to work with a single branch at a time. Use `actions/checkout` to switch to another branch.

   - otherwise, fails with `error: src refspec branch-name does not match any`

1. Defining objects:
   1. for [matrix](https://docs.github.com/en/actions/learn-github-actions/expressions#example-returning-a-json-object)
   1. for [general purpose](https://docs.github.com/en/actions/using-jobs/defining-outputs-for-jobs#example-defining-outputs-for-a-job)
      - output value should be a [string](https://docs.github.com/en/actions/learn-github-actions/contexts#needs-context)
      - read a config - [SO](https://stackoverflow.com/a/73639034)
        - sample [action](https://github.com/br4ch1st0chr0n3/nix-vscode-marketplace/actions/runs/3160375278/workflow)

1. `actions/checkout` doesn't pull the latest commit
   2. If a previous job pushes to the repo, need to pull in a current job

## Heroku

- Build a subdirectory: [buildpack](https://elements.heroku.com/buildpacks/timanovsky/subdir-heroku-buildpack)
- Deploy to Heroku: GH [action](https://github.com/marketplace/actions/deploy-to-heroku?version=v3.12.12)
- Deploy a Docker container to Heroku
  - Use [ENTRYPOINT](https://devcenter.heroku.com/articles/container-registry-and-runtime#dockerfile-commands-and-runtime)

## Docker

1. Caching [trick](https://fastapi.tiangolo.com/deployment/docker/#docker-cache) - Basically, you should copy the least volatile files like `package.json` and use them as much ASAP, and the most volatile ones like general source code as late as possible.

   - [Example](https://github.com/br4ch1st0chr0n3/devops-labs/blob/539db68da661bb9a385dbc4bb1a4bcdf6a9072b8/app_purescript/Dockerfile)

1. [Ports](https://docs.docker.com/config/containers/container-networking/#published-ports)

1. There's [docker-lock](https://github.com/safe-waters/docker-lock), but we'd better [buildLayeredImage](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/docker/examples.nix)

## Direnv

- run direnv in a separate process - [src](https://dev.to/allenap/some-direnv-best-practices-actually-just-one-4864)

- it has a wiki - [src](https://github.com/direnv/direnv/wiki)

## PureScript

1. [Halogen](https://purescript-halogen.github.io/purescript-halogen/index.html)

1. Tests

   - [purescript-spec](https://pursuit.purescript.org/packages/purescript-spec/7.0.0)
   - [example](https://github.com/citizennet/purescript-httpure/blob/1a2e1343cc272928a0e312bbe41791008089ee11/test/Test/HTTPure/BodySpec.purs#L37)

## Shell/Bash

1. Explain shell commands - [src](https://explainshell.com/explain?cmd=tar%20xzvf%20archive.tar.gz)

1. How to execute a command as if in a specific directory - [src](https://superuser.com/a/271992)

   ```sh
   (cd child && echo "hello")
   ```

1. `bash` is a superset of `sh`. So, some commands may work differently there - [src](https://www.geeksforgeeks.org/difference-between-sh-and-bash/)

1. `echo $array_name` outputs the first element of array
   1. Need to print "${array_name[@]}"

1. `set -euxo pipefail` - bash strict mode - [src](https://gist.github.com/mohanpedala/1e2ff5661761d3abd0385e8223e16425#set--o-pipefail)
   - `fish` doesn't have such flags yet - see [issue](https://github.com/fish-shell/fish-shell/issues/510)

1. `mktemp` - to create a temp file or dir - [src](https://code-maven.com/create-temporary-directory-on-linux-using-bash)

1. `xargs` - construct an argument list - [src](https://www.ibm.com/docs/en/zos/2.3.0?topic=descriptions-xargs-construct-argument-list-run-command)

## Text processing

1. `awk` is a nice tool - [src](https://www.gnu.org/software/gawk/manual/gawk.html)
   1. multiline [matches](https://stackoverflow.com/a/44547769)

1. [jq](https://www.baeldung.com/linux/jq-command-json) - for JSON
   - online [editor](https://jqplay.org/s/ekYvnaA-7IK)
   - [variables](https://stackoverflow.com/a/34747439)
   - array construction - [devdocs](https://devdocs.io/jq/index#Array/ObjectValueIterator:.[])

1. [sed](https://sed.js.org/) playground - design expressions

## VSCodium

1. We can add compound tasks in VS Code - [src](https://code.visualstudio.com/docs/editor/tasks#_compound-tasks)

1. `PAT` for GitHub should have permissions `read:user, repo, user:email, workflow` (its [checks](https://github.com/microsoft/vscode-pull-request-github/issues/3847#issue-1335886580))

## Linux

1. One can set a [cron](https://linuxhint.com/cron_jobs_complete_beginners_tutorial/) job to run e.g., `@reboot` or `@hourly`

1. [direnv](https://github.com/direnv/direnv/wiki) wiki!

1. Ignoring errors - [man](https://www.baeldung.com/linux/bash-errors)

## DevX

1. In a project, there are `solid` parts - IDE, environment, helper scripts, tasks - this should be built to make the most convenient conditions for working on `soft` parts - the code

1. Use [tmux](https://thevaluable.dev/tmux-config-mouseless/) to manage terminals

## Yandex Cloud

1. [Иерархия ресурсов Yandex Cloud](https://cloud.yandex.ru/docs/resource-manager/concepts/resources-hierarchy)
1. Deploy a container
   1. Build and load a container `back:latest`: `nix run .#backDocker`
   1. Push it to Docker Hub
   1. ssh to Yandex Cloud VM
   1. Tag, pull, run the container and expose its ports: `docker run -p 0.0.0.0:8082:8082 back:latest back`
      1. Use `0.0.0.0` to listen to any network interface - [SO](https://stackoverflow.com/a/20778887)
      1. `sudo netstat -ntlpu` should show that your app uses `0.0.0.0`
   1. Enable forwarding from docker containers to the outside world - [src](https://docs.docker.com/network/bridge/#enable-forwarding-from-docker-containers-to-the-outside-world)
   1. Buy a cheap domain on `reg.ru`, for example. Make a DNS record that maps to the VM's IP
      1. Wait, check that record using nslookup until it shows the correct IP (1h+)

## Virtual Machines

1. For local development, need to create a VM

   - e.g., [Ubuntu](https://ubuntu.com/tutorials/how-to-run-ubuntu-desktop-on-a-virtual-machine-using-virtualbox#1-overview) on VirtualBox (live-server, without GUI)
   - need to disable Secure Boot to get VirtualBox run VMs

1. Set up [port forwarding](https://dev.to/developertharun/easy-way-to-ssh-into-virtualbox-machine-any-os-just-x-steps-5d9i) in VirtualBox

1. Connect to a VM via `ssh`
   - `ssh-keygen` - generate a key
   - `ssh-copy-id` - copy it on the target VM

## Python

1. f-strings variable pprint:

   ```python
   >>> print(f"{a = }")
   a = 2
   ```

- "[Automating](https://brandonchinn178.github.io/blog/2022/05/19/automating-fourmolu-releases-with-github-actions.html) Fourmolu releases" - use Python for CI

## Kubernetes

- [Intro](https://www.youtube.com/watch?v=q_nj340pkQo)
  - There are `worker` nodes and `master` nodes. They run in a `cluster`
  - A `worker` node may run several `containers`
  - `k8s` can give access to services in a `cluster` via `DNS` or `ports`. It can perform `load balancing`
  - can attach local or remote disks to a cluster
  - automated `rollback` and `update` of `Docker images`
    - create nodes with a new image, check they work ok, kill nodes made with old images
    - `green deployment`
  - restarts containers if something happens to them

- Overview - [src](https://kubernetes.io/docs/concepts/overview/)
  - Can translate `.env` to `ConfigMap` - [src](https://humanitec.com/blog/handling-environment-variables-with-kubernetes)
    - `kubectl create configmap postgres-config --from-env-file=postgres-config.properties`
  - Can refer to data of a running instance

    ```yaml
    env:
    - name: PRODUCT_BE_SERVER_URL
      valueFrom:
          fieldRef:
            fieldPath: status.podIP
    ```

- A service is an interface to a backend

## Networks

1. [OpenVPN](https://openvpn.net/community-resources/how-to/)

   - IP address -
   - DNS names -
   - Netmask -
   - Subnets -
   - IP routing -
   - Routers -
   - Network interfaces -
   - LAN -
   - Gateways -
   - Firewall rules -

## Study

- Begin by reading the abstract and the table of contents from RFC 7230. Never neglect the table of contents: it helps you understand the scope of the document and the context of the specific parts you’ll be reading.

## Pending Questions

1. How to get size of a project in terms of its nix store paths?
