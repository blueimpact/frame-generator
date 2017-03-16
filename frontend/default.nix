{ mkDerivation, base, bytestring, containers, file-embed
, ghcjs-dom, reflex, reflex-dom, stdenv
, text, aeson
  }:

let f = { mkDerivation, base, bytestring, containers, file-embed
, frame-generator-common, ghcjs-dom, reflex, reflex-dom, stdenv
, text
}:
mkDerivation {
  pname = "frame-generator-frontend";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers file-embed frame-generator-common
    ghcjs-dom reflex reflex-dom text
  ];
  executableHaskellDepends = [ base ];
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
};

  fg = (import ../common/default.nix) {inherit mkDerivation base aeson stdenv;};
in f {inherit mkDerivation base bytestring containers file-embed ghcjs-dom
      reflex reflex-dom stdenv text; frame-generator-common = fg;}
