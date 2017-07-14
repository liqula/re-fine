{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Refine.Frontend.OrphansSpec
where

import Refine.Frontend.Prelude hiding (property)
import Refine.Frontend.Orphans ()

import Test.Hspec

import Refine.Common.Types


spec :: Spec
spec = do
  -- This test also helps in understanding what the generic jsval instances do.  Note that the data
  -- does not have to be consistent with any actual 'RawContent' value.  We're just interested in
  -- the integrity of the value when translating back and forth between haskell and js.
  --
  -- See also: tests for 'Refine.Frontend.Document.FFI.getDraftSelectionStateViaBrowser'.
  describe "instances FromJSVal, ToJSVal of SelectionState" $ do
    let chk :: Either JSString SelectionState -> JSVal -> Expectation
        chk v j = do
          when verbose $ do
            print $ either (const "Left") encode v

          j' <- toJSVal v
          -- j' == j
          case decode . cs $ js_chk j' j verbose of Just (a :: Value, b :: Value) -> a `shouldBe` b

          -- v' == v
          fromJSVal j `shouldReturn` Just v

          -- j' and j translate back to the same v
          fromJSVal j' `shouldReturn` Just v  -- (shouldn't be too surprising, but the equality test
                                              -- above is a little iffy...)

        verbose :: Bool
        verbose = False

    it "are inverses" $ do
      chk getDraftSelectionStateViaBrowser_1 js_getDraftSelectionStateViaBrowser_1
      chk getDraftSelectionStateViaBrowser_2 js_getDraftSelectionStateViaBrowser_2
      chk getDraftSelectionStateViaBrowser_3 js_getDraftSelectionStateViaBrowser_3
      chk getDraftSelectionStateViaBrowser_4 js_getDraftSelectionStateViaBrowser_4


getDraftSelectionStateViaBrowser_1 :: Either JSString SelectionState
getDraftSelectionStateViaBrowser_1 = Left "no selection"

getDraftSelectionStateViaBrowser_2 :: Either JSString SelectionState
getDraftSelectionStateViaBrowser_2 = Left "some unknown error" :: Either JSString SelectionState

getDraftSelectionStateViaBrowser_3 :: Either JSString SelectionState
getDraftSelectionStateViaBrowser_3 = Right . (`SelectionState` True) . toSelection $ Range
  (Position (BlockKey "block1") 3)
  (Position (BlockKey "block1") 3)

getDraftSelectionStateViaBrowser_4 :: Either JSString SelectionState
getDraftSelectionStateViaBrowser_4 = Right . (`SelectionState` True) . toBackwardSelection $ Range
  (Position (BlockKey "fj6g6") 266)
  (Position (BlockKey "fj6g6") 360)

#ifdef __GHCJS__

foreign import javascript safe
  "if ($3) { console.log(JSON.stringify($1)); console.log(JSON.stringify($2)); } $r = JSON.stringify([$1, $2])"
  js_chk :: JSVal -> JSVal -> Bool -> JSString

foreign import javascript safe
  "{ Left: 'no selection' }"
  js_getDraftSelectionStateViaBrowser_1 :: JSVal

foreign import javascript safe
  "{ Left: 'some unknown error' }"
  js_getDraftSelectionStateViaBrowser_2 :: JSVal

foreign import javascript safe
  "{ Right: \
       { _unSelectionState: \
           { _selectionIsBackward: false \
           , _selectionRange: \
               { _rangeBegin: { _blockIndex: 'block1', _columnIndex: 3 } \
               , _rangeEnd: { _blockIndex: 'block1', _columnIndex: 3 } \
               } \
           } \
       , _selectionStateHasFocus: true \
       } \
   }"
  js_getDraftSelectionStateViaBrowser_3 :: JSVal

foreign import javascript safe
  "{ Right: \
       { _unSelectionState: \
           { _selectionIsBackward: true \
           , _selectionRange: \
               { _rangeBegin: { _blockIndex: 'fj6g6', _columnIndex: 266 } \
               , _rangeEnd: { _blockIndex: 'fj6g6', _columnIndex: 360 } \
               } \
           } \
       , _selectionStateHasFocus: true \
       } \
   }"
  js_getDraftSelectionStateViaBrowser_4 :: JSVal

#else

{-# ANN js_chk ("HLint: ignore Use camelCase" :: String) #-}
js_chk :: JSVal -> JSVal -> Bool -> JSString
js_chk = error "javascript FFI not available in GHC"

{-# ANN js_getDraftSelectionStateViaBrowser_1 ("HLint: ignore Use camelCase" :: String) #-}
js_getDraftSelectionStateViaBrowser_1 :: JSVal
js_getDraftSelectionStateViaBrowser_1 = error "javascript FFI not available in GHC"

{-# ANN js_getDraftSelectionStateViaBrowser_2 ("HLint: ignore Use camelCase" :: String) #-}
js_getDraftSelectionStateViaBrowser_2 :: JSVal
js_getDraftSelectionStateViaBrowser_2 = error "javascript FFI not available in GHC"

{-# ANN js_getDraftSelectionStateViaBrowser_3 ("HLint: ignore Use camelCase" :: String) #-}
js_getDraftSelectionStateViaBrowser_3 :: JSVal
js_getDraftSelectionStateViaBrowser_3 = error "javascript FFI not available in GHC"

{-# ANN js_getDraftSelectionStateViaBrowser_4 ("HLint: ignore Use camelCase" :: String) #-}
js_getDraftSelectionStateViaBrowser_4 :: JSVal
js_getDraftSelectionStateViaBrowser_4 = error "javascript FFI not available in GHC"

#endif
