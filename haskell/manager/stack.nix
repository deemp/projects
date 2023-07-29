{ ghcVersion ? "928" }:
let default = ((builtins.getFlake "github:deemp/flakes/${(builtins.fromJSON (builtins.readFile ../flake.lock)).nodes.flakes.locked.rev}").outputs.makeDefault ./.);
in default.outputs.stack-shell.${builtins.currentSystem} { inherit ghcVersion; }
