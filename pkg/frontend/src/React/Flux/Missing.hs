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
data VarArg (props :: [*]) e where
  DNil :: e -> VarArg '[] e
  DCons :: (a -> VarArg as e) -> VarArg (a ': as) e

instance Functor (VarArg props) where
  fmap f (DNil a) = DNil (f a)
  fmap f (DCons g) = DCons (fmap f . g)
-------------------------------


-------------------------------
-- (ViewPropsToElement props e) is isomorphic to (VarArg props (ReactElementM e ()))
class ViewPropsToElementIso (props :: [*]) where
  isoTo :: ViewPropsToElement props e -> VarArg props (ReactElementM e ())
  isoFrom :: VarArg props (ReactElementM e ()) -> ViewPropsToElement props e

instance ViewPropsToElementIso '[] where
  isoTo = DNil
  isoFrom (DNil x) = x

instance ViewPropsToElementIso as => ViewPropsToElementIso (a ': as) where
  isoTo f = DCons (isoTo . f)
  isoFrom (DCons f) = isoFrom . f
-------------------------------


-- | like mkStatefulView but the component remembers its state when it is re-rendered
mkPersistentStatefulView
    :: forall (state :: *) (props :: [*]).
       (Typeable state, Typeable state, Eq state,
        ViewProps props ('StatefulEventHandlerCode state), Typeable props, AllEq props, ViewPropsToElementIso props)
    => JSString -- ^ A name for this view, used only for debugging/console logging
    -> LocalStateRef state -- ^ The reference of the initial state
    -> (state -> ViewPropsToElement props ('StatefulEventHandlerCode state))
    -> View props

mkPersistentStatefulView name rst trans = unsafePerformIO $ do
    st <- readIORef $ rst ^. unLocalStateRef
    pure $ mkStatefulView name st mtrans
  where
    mtrans :: state -> ViewPropsToElement props ('StatefulEventHandlerCode state)
    mtrans
      = isoFrom
      . fmap transh
      . (isoTo :: ViewPropsToElement props ('StatefulEventHandlerCode state) -> VarArg props (ReactElementM ('StatefulEventHandlerCode state) ()))
      . trans

    transh :: ReactElementM ('StatefulEventHandlerCode state) () -> ReactElementM ('StatefulEventHandlerCode state) ()
    transh = transHandler (second (trSt rst <$>) .)

{-# NOINLINE trSt #-}
trSt :: LocalStateRef state -> state -> state
trSt rst st = unsafePerformIO $ do
  writeIORef (rst ^. unLocalStateRef) st
  readIORef (rst ^. unLocalStateRef)  -- to be sure that writeIORef is not optimized out
