{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    src = ./src;
    frontend = ./app;
  };

  shells = {
    ghcjs = ["src" "frontend" ];
  };

  overrides = self: super: {
    graphviz = self.callHackage "graphviz" "2099.20.1.0" {};
  };
})
