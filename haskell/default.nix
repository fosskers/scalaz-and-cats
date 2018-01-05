{ mkDerivation, base, criterion, mtl, stdenv, text, text-show }:
mkDerivation {
  pname = "scalaz-and-cats";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  benchmarkHaskellDepends = [ base criterion mtl text text-show ];
  homepage = "https://github.com/fosskers/scalaz-and-cats";
  license = stdenv.lib.licenses.bsd3;
}
