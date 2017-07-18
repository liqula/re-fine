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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Test.Marshal where

import Refine.Frontend.Prelude hiding (property)

import Test.QuickCheck
import Test.Hspec
import Data.Aeson (encode, eitherDecode)


checkJSValJSON :: forall proxy a.
                  (Typeable (proxy a), Show a, Eq a, Arbitrary a, FromJSON a, ToJSON a, FromJSVal a, ToJSVal a)
               => proxy a -> Spec
checkJSValJSON proxy = describe ("checkJSValJSON: " <> show (typeOf proxy)) $ do
  let debugMode :: Bool = False

  it "hs.ToJSON -> hs.FromJSON" . property $ \(v :: a) -> do
    eitherDecode (encode v) `shouldBe` Right v

  it "hs.ToJSVal -> hs.FromJSVal" . property $ \(v :: a) -> do
    v' <- toJSVal v
    v'' <- fromJSVal v'
    v'' `shouldBe` Just v

  it "hs.ToJSON -> js.JSON.parse -> hs.FromJSVal" . property $ \(v :: a) -> do
    when debugMode $ do
      liftIO . print . js_jsonStringify debugMode =<< toJSVal v
    v' <- fromJSVal . js_jsonParse debugMode . cs . encode $ v
    v' `shouldBe` Just v

  it "hs.ToJSVal -> js.JSON.stringify -> hs.FromJSON" . property $ \(v :: a) -> do
    v' <- eitherDecode . cs . js_jsonStringify debugMode <$> toJSVal v
    v' `shouldBe` Right v

#ifdef __GHCJS__

foreign import javascript safe
  "(function() { if ($1) { console.log($2, JSON.stringify($2)); } return JSON.stringify($2); })()"
  js_jsonStringify :: Bool -> JSVal -> JSString

foreign import javascript safe
  "(function() { if ($1) { console.log($2, JSON.parse($2)); } return JSON.parse($2); })()"
  js_jsonParse :: Bool -> JSString -> JSVal

#else

{-# ANN js_jsonStringify ("HLint: ignore Use camelCase" :: String) #-}
js_jsonStringify :: Bool -> JSVal -> JSString
js_jsonStringify = error "javascript FFI not available in GHC"

{-# ANN js_jsonParse ("HLint: ignore Use camelCase" :: String) #-}
js_jsonParse :: Bool -> JSString -> JSVal
js_jsonParse = error "javascript FFI not available in GHC"

#endif
