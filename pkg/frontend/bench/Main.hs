{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import           Control.DeepSeq
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS (writeFile, readFile)
import           Data.Monoid
import           Data.Time.Clock
import           System.IO
import           Test.QuickCheck

import Refine.Common.Test.Arbitrary ()
import Refine.Common.Types
import Refine.Frontend.Document.FFI
import Refine.Frontend.Util (js_getScrollOffset)

{-
-- bug: criterion-1.2.0.0 use some C foreign functions not defined with ghcjs

import Criterion.Main

main :: IO ()
main = defaultMain [
    bench "getScrollOffset" $ nfIO js_getScrollOffset
  ]
-}

main :: IO ()
main = do
  rawContentConversionJSValVsAesonStrings Run
--  foreignFunctionSafeVsUnsafe


printWithMsg :: String -> Show a => a -> IO ()
printWithMsg msg a = hPutStrLn stderr $ ". >>> " <> msg <> ": " <> show a

takeTimeCheapAss :: String -> IO () -> IO ()
takeTimeCheapAss msg action = do
  hPutStr stderr "."
  t1 <- getCurrentTime
  () <- action
  hPutStr stderr "."
  t2 <- getCurrentTime
  printWithMsg msg $ diffUTCTime t2 t1

takeTimeCheapAssPure :: String -> NFData a => (a -> b) -> a -> IO b
takeTimeCheapAssPure msg aggregator expr = do
  takeTimeCheapAss msg $ expr `deepseq` pure ()
  pure $ aggregator expr


data Mode = Generate | Run
  deriving (Eq, Show)


{- The old code (Aeson.encode/decode in Haskell, JSON.parse/stringify on js) seems to be even slightly
faster in one direction, but slower by a factor of almost 2 in the other.

via JSString (with JSON.parse on js side):
... >>> convertFromRaw: 40.348s
... >>> convertToRaw: 86.806s
... >>> convertFromRaw: 41.804s
... >>> convertToRaw: 85.669s
... >>> convertFromRaw: 40.762s
... >>> convertToRaw: 89.273s

via JSVal:
... >>> convertFromRaw: 42.391s
... >>> convertToRaw: 48.564s
... >>> convertFromRaw: 42.208s
... >>> convertToRaw: 51.303s

-}
rawContentConversionJSValVsAesonStrings :: Mode -> IO ()
rawContentConversionJSValVsAesonStrings Generate = do
  let sampleSize = 10
  LBS.writeFile "bench/rawContentConversionJSValVsAesonStrings.json" . encode
      =<< generate (vectorOf sampleSize (arbitrary @RawContent))

rawContentConversionJSValVsAesonStrings Run = do
  let sampleSize = 1000
  rcs :: [RawContent]
     <- either error (take sampleSize . cycle) .
        eitherDecode <$>
        LBS.readFile "bench/rawContentConversionJSValVsAesonStrings.json"
  rcs `deepseq` do
    ints <- takeTimeCheapAssPure "convertFromRaw" id         $ convertFromRaw <$> rcs
    ()   <- takeTimeCheapAssPure "convertToRaw"   (const ()) $ convertToRaw   <$> ints
    pure ()


-- foreign import javascript unsafe "(function() { return pageYOffset; })()"    1000000 call   1.56s
-- foreign import javascript unsafe "$r = pageYOffset"                          1000000 call   1.50s
-- foreign import javascript safe "$r = pageYOffset"                            1000000 call   1.53s
foreignFunctionSafeVsUnsafe :: IO ()
foreignFunctionSafeVsUnsafe = takeTimeCheapAss "foreignFunctionSafeVsUnsafe" $ do
  mapM_ (\_ -> js_getScrollOffset) [1 :: Int ..1000000]
