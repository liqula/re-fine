{ mkDerivation, aeson, base, basic-sop, blaze-html, blaze-markup
, checkers, containers, deepseq, fingertree-tf, generics-sop, hspec
, hspec-checkers, hspec-core, hspec-golden-aeson, http-api-data
, i18n, json-sop, lens, monoid-extras, mtl, natural-transformation
, patience, QuickCheck, quickcheck-instances, refine-prelude
, servant, stdenv, string-conversions, text, time, transformers
, unordered-containers, vector, xml-html-qq
}:
mkDerivation {
  pname = "refine-common";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base basic-sop checkers containers deepseq fingertree-tf
    generics-sop hspec hspec-core http-api-data i18n json-sop lens
    monoid-extras mtl natural-transformation patience QuickCheck
    quickcheck-instances refine-prelude servant string-conversions text
    time transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base basic-sop blaze-html blaze-markup checkers containers
    deepseq fingertree-tf generics-sop hspec hspec-checkers hspec-core
    hspec-golden-aeson http-api-data i18n json-sop lens monoid-extras
    mtl natural-transformation patience QuickCheck quickcheck-instances
    refine-prelude servant string-conversions text time transformers
    unordered-containers vector xml-html-qq
  ];
  homepage = "https://github.com/fisx/refine";
  description = "Refine common parts for frontend and backend";
  license = "AGPL";
}
