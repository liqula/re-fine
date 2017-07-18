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
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module React.Flux.Missing
  ( LocalStateRef(..)
  , unLocalStateRef
  , newLocalStateRef
  , mkPersistentStatefulView
  ) where

import Refine.Frontend.Prelude

import           Control.DeepSeq
import           Data.IORef
import           System.IO.Unsafe
import           GHC.Generics (Generic(..), Rec0)


newtype LocalStateRef a = LocalStateRef {_unLocalStateRef :: IORef a}
  deriving (Eq)

-- | Note that newLocalStateRef breaks referential transparency:
--
-- (newLocalStateRef x, newLocalStateRef x)
--
-- is not the same as
--
-- let r = newLocalStateRef x in (r, r)
--
-- Usually you need second one, so try to call newLocalStateRef as early as possible
-- and share its result.
{-# NOINLINE newLocalStateRef #-}
newLocalStateRef :: a -> LocalStateRef a
newLocalStateRef a = unsafePerformIO (LocalStateRef <$> newIORef a)

instance NFData (LocalStateRef a) where rnf x = x `seq` ()

-- FIXME: remove dummy instance
instance FromJSON (LocalStateRef a) where
  parseJSON = error "parseJSON @LocalStateRef"
-- FIXME: remove dummy instance
instance ToJSON (LocalStateRef a) where
  toJSON = error "toJSON @LocalStateRef"
-- FIXME: remove dummy instance
instance Generic (LocalStateRef a) where
  type Rep (LocalStateRef a) = Rec0 ()
  from = error "from @LocalStateRef"
  to = error "to @LocalStateRef"

instance Show (LocalStateRef a) where
  show _ = "LocalStateRef _"

deriveClasses
  [ ([''LocalStateRef], [''Lens'])
  ]


-- | like mkStatefulView but the component remembers its state when it is re-rendered
-- TODO: generalize this to match mkStatefulView
mkPersistentStatefulView
    :: forall state .
       (Typeable state, Typeable state, Eq state,
        ViewProps '[] ('StatefulEventHandlerCode state))
    => JSString -- ^ A name for this view, used only for debugging/console logging
    -> LocalStateRef state -- ^ The initial state
    -> (state -> ReactElementM ('StatefulEventHandlerCode state) ())
    -> View '[]
mkPersistentStatefulView name rst trans = unsafePerformIO $ do
    st <- readIORef $ rst ^. unLocalStateRef
    pure $ mkStatefulView name st mtrans
  where
    mtrans :: state -> ReactElementM_ (state -> ([SomeStoreAction], Maybe state)) ()
    mtrans = transHandler tr . trans
      where
        tr :: (state -> ([SomeStoreAction], Maybe state)) -> state -> ([SomeStoreAction], Maybe state)
        tr f = second (trSt rst <$>) . f

{-# NOINLINE trSt #-}
trSt :: LocalStateRef state -> state -> state
trSt rst st = unsafePerformIO $ do
  writeIORef (rst ^. unLocalStateRef) st
  readIORef (rst ^. unLocalStateRef)  -- to be sure that writeIORef is not optimized out
