{ mkDerivation, aeson, base, basic-sop, bytestring, containers
, data-default, deepseq, directory, filepath, generics-sop
, ghcjs-base, hspec, hspec-core, hspec-golden-aeson, http-api-data
, i18n, json-sop, language-css, lens, mtl, pretty, pretty-show
, process, QuickCheck, quickcheck-instances, random, react-hs
, refine-common, refine-prelude, stdenv, string-conversions, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "refine-frontend";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base basic-sop containers data-default deepseq generics-sop
    ghcjs-base hspec http-api-data i18n json-sop language-css lens mtl
    pretty-show QuickCheck quickcheck-instances random react-hs
    refine-common refine-prelude string-conversions text time
    transformers unordered-containers
  ];
  executableHaskellDepends = [ base ghcjs-base react-hs ];
  testHaskellDepends = [
    aeson base basic-sop bytestring containers deepseq directory
    filepath ghcjs-base hspec hspec-core hspec-golden-aeson
    http-api-data i18n json-sop language-css lens pretty process
    QuickCheck quickcheck-instances react-hs refine-common
    refine-prelude string-conversions text time unordered-containers
  ];
  benchmarkHaskellDepends = [
    aeson base bytestring containers deepseq QuickCheck refine-common
    string-conversions time
  ];
  homepage = "https://github.com/fisx/refine";
  description = "Refine frontend";
  license = "AGPL";
}
