{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.ConfigSpec where
#include "import_backend.hs"

import Test.Hspec
import Refine.Backend.Config


spec :: Spec
spec = do
  describe "WarpSettings" $ do
    it "aeson encode and decode are inverses" $ do
      let test :: WarpSettings -> String -> Expectation
          test x s = do
            show x `shouldBe` s
            Aeson.eitherDecode (Aeson.encode x) `shouldBe` Right x

      test (WarpSettings 3000 "127.0.0.1")
        "WarpSettings {_warpSettingsPort = 3000, _warpSettingsHost = Host \"127.0.0.1\"}"

      test (WarpSettings 3000 "*")   -- means HostAny - "any IPv4 or IPv6 hostname"
        "WarpSettings {_warpSettingsPort = 3000, _warpSettingsHost = HostAny}"

      test (WarpSettings 3000 "*4")  -- means HostIPv4 - "any IPv4 or IPv6 hostname, IPv4 preferred"
        "WarpSettings {_warpSettingsPort = 3000, _warpSettingsHost = HostIPv4}"

      test (WarpSettings 3000 "!4")  -- means HostIPv4Only - "any IPv4 hostname"
        "WarpSettings {_warpSettingsPort = 3000, _warpSettingsHost = HostIPv4Only}"

      test (WarpSettings 3000 "*6")  -- means HostIPv6@ - "any IPv4 or IPv6 hostname, IPv6 preferred"
        "WarpSettings {_warpSettingsPort = 3000, _warpSettingsHost = HostIPv6}"

      test (WarpSettings 3000 "!6")  -- means HostIPv6Only - "any IPv6 hostname"
        "WarpSettings {_warpSettingsPort = 3000, _warpSettingsHost = HostIPv6Only}"
