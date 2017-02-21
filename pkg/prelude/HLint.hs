-- to turn off unreasonable hints on a case-by-case basis, consult
-- https://github.com/ndmitchell/hlint#ignoring-hints

import Debug.Trace

import Data.String (fromString)
import Data.String.Conversions (cs)

import "hlint" HLint.Default
import "hlint" HLint.Dollar
import "hlint" HLint.Generalise
import "hlint" HLint.HLint

ignore "Redundant do"
ignore "Use const"
ignore "Use fmap"
ignore "Use list literal"
ignore "Use record patterns"
ignore "Unused LANGUAGE pragma"  -- tried to that with `{-# ANN module ... #-}`, but then ghc-8.0.1
                                 -- couldn't find the module keyword any more.
ignore "Use =<<" -- Do not change concatMap into =<< for sake of readibility.

warn = listToMaybe (filter f xs) ==> find f xs
warn = isJust $ find f xs ==> any f xs
warn = return ==> pure
warn = f ^. g ^. h ==> f ^. g . h
warn = fx f ^. g ==> f ^. to fx . g
warn = fromString ==> cs -- Refine.Prelude module should be imported to support this

-- this is a bit of a hack, but it works.  use "ignore Use errorDoNotUseTrace" in annotations.
warn = trace         ==> errorDoNotUseTrace
warn = traceId       ==> errorDoNotUseTrace
warn = traceShow     ==> errorDoNotUseTrace
warn = traceShowId   ==> errorDoNotUseTrace
warn = traceStack    ==> errorDoNotUseTrace
warn = traceIO       ==> errorDoNotUseTrace
warn = traceShowM    ==> errorDoNotUseTrace
warn = traceEventIO  ==> errorDoNotUseTrace
warn = traceEvent    ==> errorDoNotUseTrace
warn = traceMarker   ==> errorDoNotUseTrace
warn = traceMarkerIO ==> errorDoNotUseTrace
