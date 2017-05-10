{ mkDerivation, aeson, base, data-default, stdenv, text }:
mkDerivation {
  pname = "frame-generator-common";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base data-default text ];
  license = stdenv.lib.licenses.unfree;
}
