-- to turn off unreasonable hints on a case-by-case basis, consult
-- https://github.com/ndmitchell/hlint#ignoring-hints

import           Data.String (fromString)
import           Data.String.Conversions (cs)
import qualified Data.Text as ST
import           Debug.Trace
import           Safe

import "hlint" HLint.Default
import "hlint" HLint.Dollar
import "hlint" HLint.Generalise
import "hlint" HLint.HLint

ignore "Redundant do"
ignore "Unused LANGUAGE pragma"  -- tried to that with `{-# ANN module ... #-}`, but then ghc-8.0.1
                                 -- couldn't find the module keyword any more.
ignore "Use &&"
ignore "Use const"
ignore "Use =<<" -- Do not change concatMap into =<< for sake of readibility.
ignore "Use fmap"
ignore "Use fromMaybe"
ignore "Use list literal"
ignore "Use record patterns"
ignore "Use String"

warn = listToMaybe (filter f xs) ==> find f xs
warn = isJust $ find f xs ==> any f xs
warn = return ==> pure
warn = f ^. g ^. h ==> f ^. g . h
warn = fx f ^. g ==> f ^. to fx . g
warn = fromString ==> cs -- Refine.Prelude module should be imported to support this
warn = ST.intercalate " " ==> ST.unwords

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

warn = fromJust      ==> fromJustNote
warn = head          ==> headMay
warn = last          ==> lastMay
warn = (!!)          ==> atMay
warn = a !! b        ==> atMay  -- like @let Just x = a `atMay` b in x@: pattern match failures
                                -- produce easily locatable errors.
