{-# LANGUAGE BangPatterns               #-}
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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Test.Console
  ( consoleLogJSVal
  , consoleLogJSON
  , consoleLogReactWrapper
  , consoleLogShallowWrapper
  , consoleLogStringified
  )
where

import Data.Aeson (ToJSON, encode)
import Data.JSString (JSString, pack)
import Data.String.Conversions (cs)
import GHCJS.Types (JSVal)
import Refine.Frontend.Test.Enzyme.ReactWrapperAPI


consoleLogShallowWrapper :: JSString -> ShallowWrapper -> IO ()
consoleLogShallowWrapper msg (ShallowWrapper jsval) = consoleLogJSVal msg jsval

consoleLogReactWrapper :: JSString -> ReactWrapper -> IO ()
consoleLogReactWrapper msg (ReactWrapper jsval) = consoleLogJSVal msg jsval


-- | Write a 'JSVal' to stdout (node) or the console (browser).  (Similar to 'consoleLogJSON', but
-- because it doesn't use `JSON.parse`, it is faster and the encoding caveat does not apply.)
foreign import javascript unsafe
  "console.log($1, $2);"
  consoleLogJSVal :: JSString -> JSVal -> IO ()

-- | Write a 'JSVal' to stdout (node) or the console (browser) via JSON.stringify() which allows
-- for deep rendering of the object.
-- Caution: Does not work for circular objects (i.e. usually ShallowWrappers).
foreign import javascript unsafe
  "console.log($1, JSON.stringify($2));"
  consoleLogStringified :: JSString -> JSVal -> IO ()

-- | Write a 'ToJSON' instance to stdout (node) or the console (browser).  If you have a choice, use
-- 'consoleLogJSVal' which is more efficient.  (No idea if there are char encoding issues here.  But
-- it's probably safe to use it for development.)
consoleLogJSON :: ToJSON a => JSString -> a -> IO ()
consoleLogJSON str state = consoleLog_ str ((pack . cs . encode) state)

foreign import javascript unsafe
  "console.log($1, $2);"
  consoleLog_ :: JSString -> JSString -> IO ()
