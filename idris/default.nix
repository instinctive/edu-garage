{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  idris2 = pkgs.idris2;
in
mkShell {
  buildInputs = [
    idris2
    chez   # Chez Scheme, which Idris 2 uses as its default code generator
    gmp    # GNU Multiple Precision Arithmetic Library
    rlwrap # for command-line history (readline wrapper)
    zlib   # Compression library
  ];

  shellHook = ''
    echo "Idris 2 development environment loaded"
    echo "Idris 2 version: $(idris2 --version)"
    echo
    echo "To have readline editing: $ rlwrap idris2 ..."
  '';
}
