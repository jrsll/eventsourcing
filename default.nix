{ mkDerivation, aeson, base, ghc-mod, hindent, hlint, QuickCheck
, stdenv, time, transformers
}:
mkDerivation {
  pname = "eventsourcing";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base ghc-mod hindent hlint time transformers
  ];
  testHaskellDepends = [ aeson base QuickCheck time transformers ];
  homepage = "https://github.com/jrsll/eventsourcing#readme";
  license = stdenv.lib.licenses.bsd3;
}
