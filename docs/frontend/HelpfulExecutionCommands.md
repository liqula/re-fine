
## General

`{-# OPTIONS_GHC -w #-}` turns off all warnings. This must be inserted above the module declaration. Never commit this!!


## Compiling

* `stack build --fast --no-test --flag refine-frontend:-ghc-warn-error && make` to explore things in the running application


## Running the tests

* `stack test --fast` is the go-to command
* Add `--test-arguments '--match=###'` to select all tests whose description contains `###`
* Add `--flag refine-frontend:-ghc-warn-error--ghc-options -Wwarn` if you can't be bothered to fix warnings right now
* `../../build` is the ultimate command before checking in (does everything, takes a long time).


## random sources of information for ghcjs

https://www.reddit.com/r/haskell/comments/6bqfk9/testing_ghc_release_candidates_with_stack/
