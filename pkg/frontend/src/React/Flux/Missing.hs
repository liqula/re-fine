{-# LANGUAGE AllowAmbiguousTypes        #-}
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
{-# LANGUAGE TypeFamilyDependencies     #-}
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

import           React.Flux.Internal
import           Control.DeepSeq
import qualified Control.Lens as Lens
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

------------------------------- vararg functions
type family VarArg (props :: [*]) e where
  VarArg '[] e = e
  VarArg (a ': as) e = a -> VarArg as e

------------------------------- fmap over vararg functions
-- see https://stackoverflow.com/questions/45178068/fmap-over-variable-argument-function/
mapVarArg :: forall props e e' . VarArgIso props => (e -> e') -> VarArg props e -> VarArg props e'
mapVarArg f = Lens.under (Lens.from (varArgIso @props)) (fmap f)

data VarArgD (props :: [*]) e where
  DNil  :: e -> VarArgD '[] e
  DCons :: (a -> VarArgD as e) -> VarArgD (a ': as) e

class VarArgIso (props :: [*]) where
  varArgIso :: Lens.Iso (VarArg props e) (VarArg props e') (VarArgD props e) (VarArgD props e')

instance VarArgIso '[] where
  varArgIso = iso DNil (\(DNil x) -> x)

instance VarArgIso as => VarArgIso (a ': as) where
  varArgIso = iso (\f -> DCons ((^. varArgIso) . f)) (\(DCons f) -> ((^. Lens.from varArgIso) . f))

instance Functor (VarArgD props) where
  fmap f (DNil a)  = DNil (f a)
  fmap f (DCons g) = DCons (fmap f . g)
-------------------------------

-- | like mkStatefulView but the component remembers its state when it is re-rendered
mkPersistentStatefulView
    :: forall (state :: *) (props :: [*]).
       (Typeable state, Typeable state, Eq state,
        ViewProps props ('StatefulEventHandlerCode state), Typeable props, AllEq props
       , VarArgIso props

       -- FIXME (react-hs):
       -- define
       --   type ViewPropsToElement props code = VarArg props (ReactElementM code)
       -- so the next line will not be needed
       , ViewPropsToElement props ('StatefulEventHandlerCode state)
         ~ VarArg props (ReactElementM (('StatefulEventHandlerCode state)) ())
       )
    => JSString -- ^ A name for this view, used only for debugging/console logging
    -> LocalStateRef state -- ^ The reference of the initial state
    -> (state -> ViewPropsToElement props ('StatefulEventHandlerCode state))
    -> View props

mkPersistentStatefulView name rst trans = unsafePerformIO $ do
    st <- readIORef $ rst ^. unLocalStateRef
    pure $ mkStatefulView name st mtrans
  where
    mtrans :: state -> ViewPropsToElement props ('StatefulEventHandlerCode state)
    mtrans = mapVarArg @props transh . trans

    transh :: ReactElementM ('StatefulEventHandlerCode state) () -> ReactElementM ('StatefulEventHandlerCode state) ()
    transh = transHandler (second (trSt rst <$>) .)

{-# NOINLINE trSt #-}
trSt :: LocalStateRef state -> state -> state
trSt rst st = unsafePerformIO $ do
  writeIORef (rst ^. unLocalStateRef) st
  readIORef (rst ^. unLocalStateRef)  -- to be sure that writeIORef is not optimized out
