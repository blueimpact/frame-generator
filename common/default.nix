{ mkDerivation, aeson, base, stdenv }:
mkDerivation {
  pname = "frame-generator-common";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ aeson base ];
  license = stdenv.lib.licenses.bsd3;
}
