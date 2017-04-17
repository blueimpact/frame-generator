{ mkDerivation
, base
, bytestring
, containers
, file-embed
, ghcjs-dom
, reflex
, reflex-dom
, stdenv
, text
, aeson
, base64-bytestring
, bifunctors
, data-default
, ghc
, ghcjs-base
, http-types
, lens
, mtl
, random
, readable
, safe
, semigroups
, string-conv
, these
, time
, transformers
, uri-bytestring
, webkitgtk3-javascriptcore
  }:

let f = { mkDerivation, base, bytestring, containers, file-embed
, frame-generator-common, ghcjs-dom, reflex, reflex-dom, stdenv
, text, reflex-dom-contrib, uri-bytestring, lens
}:
mkDerivation {
  pname = "frame-generator-frontend";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers file-embed frame-generator-common
    ghcjs-dom reflex reflex-dom text reflex-dom-contrib uri-bytestring lens
  ];
  executableHaskellDepends = [ base ];
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
};

  fg = (import ../common/default.nix) {inherit mkDerivation base aeson stdenv;};
  rc = (import ./reflex-dom-contrib) {inherit mkDerivation aeson base64-bytestring bifunctors
          data-default ghc ghcjs-base ghcjs-dom http-types lens mtl random readable reflex
          reflex-dom safe semigroups string-conv text these time transformers uri-bytestring webkitgtk3-javascriptcore; };

in f {inherit mkDerivation base bytestring containers file-embed ghcjs-dom
      reflex reflex-dom stdenv text uri-bytestring lens;
      frame-generator-common = fg;
      reflex-dom-contrib = rc;}
