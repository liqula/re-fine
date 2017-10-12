{ mkDerivation, aeson, base, basic-sop, bytestring, containers
, data-default, deepseq, directory, generics-sop, hspec, hspec-core
, http-api-data, json-sop, lens, monad-control, mtl
, natural-transformation, process, QuickCheck, quickcheck-instances
, safe, servant, stdenv, stm, string-conversions, template-haskell
, text, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "refine-prelude";
  version = "0.0.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base basic-sop bytestring containers data-default deepseq
    directory generics-sop http-api-data json-sop lens monad-control
    mtl natural-transformation process safe servant stm
    string-conversions template-haskell text time transformers
    unordered-containers
  ];
  testHaskellDepends = [
    aeson base basic-sop bytestring containers data-default deepseq
    directory generics-sop hspec hspec-core http-api-data json-sop lens
    monad-control mtl natural-transformation process QuickCheck
    quickcheck-instances safe servant stm string-conversions
    template-haskell text time transformers unordered-containers
  ];
  license = "AGPL";
}
