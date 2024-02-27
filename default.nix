with import <nixpkgs> { };

mkShell rec {
    buildInputs = [ zlib ];

    LD_LIBRARY_BATH = lib.makeLibraryPath buildInputs;
}
