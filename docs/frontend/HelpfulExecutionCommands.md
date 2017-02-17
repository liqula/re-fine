
## General

`{-# OPTIONS_GHC -w #-}` turns off all warnings. This must be inserted above the module declaration. Never commit this!!


## Compiling

* `stack build --fast && make` to explore things in the running application


## Running the tests

* `stack test --fast` is the go-to command
* Add `--test-arguments '--match=###'` to select all tests whose description contains `###`

* `stack test --fast && ../../build hlint && make` is the ultimate command before checking in
