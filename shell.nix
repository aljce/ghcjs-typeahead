with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "ghcjs-typeahead";
  ghc = haskellPackages.ghc;
  shellHook = "export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt";
  buildInputs =
    [ zlib
      nodejs
      haskellPackages.happy
      ncurses
      cabal-install
      git ];
}
