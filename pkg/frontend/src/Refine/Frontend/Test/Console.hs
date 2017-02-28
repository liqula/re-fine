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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Frontend.Test.Console
  ( consoleLog
  , consoleLogJSON
  , consoleLogStringified
  )
where

import Data.Aeson (ToJSON, encode)
import Data.JSString (JSString, pack)
import Data.String.Conversions (cs)
import GHCJS.Types (JSVal)


-- | Write a 'JSString' to stdout (node) or the console (browser).
foreign import javascript unsafe
  "console.log($1, $2);"
  consoleLog :: JSString -> JSString -> IO ()

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
consoleLogJSON str state = consoleLog str ((pack . cs . encode) state)
