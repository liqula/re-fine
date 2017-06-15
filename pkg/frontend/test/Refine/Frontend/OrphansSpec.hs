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
  -- this test also helps in understanding what the generic jsval instances do.
  describe "instances FromJSVal, ToJSVal of SelectionState" $ do
    let chk :: Either JSString SelectionState -> JSVal -> Expectation
        chk v j = do
          j' <- toJSVal v
          -- j' == j
          case decode . cs $ js_chk j' j verbose of Just (a :: Value, b :: Value) -> a `shouldBe` b

          -- v' == v
          fromJSVal j `shouldReturn` Just v

          -- j' and j translate back to the same v
          fromJSVal j' `shouldReturn` Just v  -- (shouldn't be too surprising, but teh equality test
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
getDraftSelectionStateViaBrowser_3 = Right (SelectionState False (SelectionPoint (BlockKey "block1") 3) (SelectionPoint (BlockKey "block1") 3))

getDraftSelectionStateViaBrowser_4 :: Either JSString SelectionState
getDraftSelectionStateViaBrowser_4 = Right (SelectionState True (SelectionPoint (BlockKey "fj6g6") 266) (SelectionPoint (BlockKey "fj6g6") 360))

#ifdef __GHCJS__

foreign import javascript safe
  "if ($3) { console.log($1); console.log($2); } $r = JSON.stringify([$1, $2])"
  js_chk :: JSVal -> JSVal -> Bool -> JSString

foreign import javascript safe
  "{ Left: 'no selection' }"
  js_getDraftSelectionStateViaBrowser_1 :: JSVal

foreign import javascript safe
  "{ Left: 'some unknown error' }"
  js_getDraftSelectionStateViaBrowser_2 :: JSVal

foreign import javascript safe
  "{ Right: { _selectionIsBackward: false, _selectionStart: {_selectionBlock: 'block1', _selectionOffset: 3}, _selectionEnd: {_selectionBlock: 'block1', _selectionOffset: 3} } }"
  js_getDraftSelectionStateViaBrowser_3 :: JSVal

foreign import javascript safe
  "{Right:{_selectionIsBackward:true,_selectionStart:{_selectionBlock:\"fj6g6\",_selectionOffset:266},_selectionEnd:{_selectionBlock:\"fj6g6\",_selectionOffset:360}}}"
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
