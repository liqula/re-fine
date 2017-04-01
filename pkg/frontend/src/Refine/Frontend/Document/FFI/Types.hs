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


module Refine.Frontend.Document.FFI.Types
  ( EditorState(..)
  , ContentState(..)
  , unEditorState
  , unsafeMkEditorState
  ) where

import           GHC.Generics (Generic)
import           GHCJS.Types (JSVal)

import           Refine.Frontend.Util (js_eq)
import           Refine.Prelude.Aeson (NoJSONRep(NoJSONRep))
import           Refine.Prelude.TH (makeRefineType)


newtype EditorState = EditorState (NoJSONRep JSVal)
  deriving (Show, Generic)

instance Eq EditorState where
  EditorState (NoJSONRep js) == EditorState (NoJSONRep js') = js_eq js js'  -- (not too confident about this one...)

newtype ContentState = ContentState (NoJSONRep JSVal)
  deriving (Show, Generic)

makeRefineType ''EditorState
makeRefineType ''ContentState


unEditorState :: EditorState -> JSVal
unEditorState (EditorState (NoJSONRep jsval)) = jsval

unsafeMkEditorState :: JSVal -> EditorState
unsafeMkEditorState = EditorState . NoJSONRep
