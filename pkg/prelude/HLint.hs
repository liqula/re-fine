-- to turn off unreasonable hints on a case-by-case basis, consult
-- https://github.com/ndmitchell/hlint#ignoring-hints

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

warn = listToMaybe (filter f xs) ==> find f xs
warn = isJust $ find f xs ==> any f xs
