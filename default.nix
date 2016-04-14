with import <nixpkgs> { };
let f = ./Main.hs;
in
runCommand "puffyterm" {
  buildInputs = [
    (haskellPackages.ghcWithPackages (p: [ (haskell.lib.addPkgconfigDepends p.vte [ glib pango gtk ]) ]))
  ];
} ''
  mkdir -p $out/bin
  ghc -o $out/bin/puffyterm ${f}
''
