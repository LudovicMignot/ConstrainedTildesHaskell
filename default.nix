{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    src = ./src;
    frontend = ./app;
  };

  shells = {
    ghc = ["src" "frontend"];
    ghcjs = ["src" "frontend" ];
  };

       overrides = self: super: {
         singletons = self.callHackage "singletons" "2.7" {};     
         th-desugar = self.callHackage "th-desugar" "1.11" {};         
       };

})
