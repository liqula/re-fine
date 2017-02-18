-- to turn off unreasonable hints on a case-by-case basis, consult
-- https://github.com/ndmitchell/hlint#ignoring-hints

import Debug.Trace

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

-- trace turned into an error call, it is only for signaling the
-- leftover trace-s, not to rewrite them.
warn = trace ==> error "trace should not be used"
warn = traceId ==> error "trace should not be used"
warn = traceShow ==> error "trace should not be used"
warn = traceShowId ==> error "trace should not be used"
warn = traceStack ==> error "trace should not be used"
warn = traceIO ==> error "trace should not be used"
warn = traceShowM ==> error "trace should not be used"
warn = traceEventIO ==> error "trace should not be used"
warn = traceEvent ==> error "trace should not be used"
warn = traceMarker ==> error "trace should not be used"
warn = traceMarkerIO ==> error "trace should not be used"
