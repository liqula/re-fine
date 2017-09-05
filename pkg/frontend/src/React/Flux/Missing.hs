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
  ( LocalStateRef
  , newLocalStateRef
  , newLocalStateRefM
  , readLocalStateRef
  , unsafeReadLocalStateRef
  , mkPersistentStatefulView
  ) where

import Refine.Frontend.Prelude

import           React.Flux.Internal
import qualified Control.Lens as Lens
import           Data.IORef
import           System.IO.Unsafe

newtype LocalStateRef a = LocalStateRef {_unLocalStateRef :: NoJSONRep (IORef a)}
  deriving (Eq, Show, Generic)

deriveClasses [([''LocalStateRef], allClass)]

-- | Note that newLocalStateRef breaks referential transparency:
--
--     (newLocalStateRef x, newLocalStateRef x)
--
-- is not the same as
--
--     let r = newLocalStateRef x in (r, r)
--
-- Usually you need second one, so try to call newLocalStateRef as early as possible
-- and share its result.
--
-- The second parameter of newLocalStateRef can be used to prevent cse (common sub-expression
-- elimination) and let-floating at the call side. Without this,
--
--     [newLocalStateRef 1 | i <- [1..10]]
--
-- is likely to be simplified to
--
--     let r = newLocalStateRef 1 in [r | i <- [1..10]]
--
-- or
--
--     (newLocalStateRef x, newLocalStateRef x)
--
-- is likely to be simplified to
--
--     let r = newLocalStateRef x in (r, r)
--
-- The solution is:
--
--     [newLocalStateRef 1 i | i <- [1..10]]
--
--     (newLocalStateRef x 'l', newLocalStateRef x 'r')
--
-- or use 'newLocalStateRefM'.
{-# NOINLINE newLocalStateRef #-}
newLocalStateRef :: a -> b -> LocalStateRef a
newLocalStateRef a _ = unsafePerformIO (LocalStateRef . NoJSONRep <$> newIORef a)

-- | See 'newLocalStateRef'.
{-# NOINLINE newLocalStateRefM #-}
newLocalStateRefM :: Monad m => a -> m (LocalStateRef a)
newLocalStateRefM a = pure $ newLocalStateRef a ()

readLocalStateRef :: LocalStateRef a -> IO a
readLocalStateRef (LocalStateRef (NoJSONRep ref)) = readIORef ref

{-# NOINLINE unsafeReadLocalStateRef #-}
unsafeReadLocalStateRef :: LocalStateRef a -> a
unsafeReadLocalStateRef = unsafePerformIO . readLocalStateRef


-- * vararg function representations

-- | type family representation: easy to use (outside of this module), hard to work with (in this module)
type family VarArg (args :: [*]) e where
  VarArg '[] e = e
  VarArg (a ': as) e = a -> VarArg as e

-- | GADT representation: hard to use (outside of this module), easy to work with (in this module)
data VarArgGADT (args :: [*]) e where
  DNil  :: e -> VarArgGADT '[] e
  DCons :: (a -> VarArgGADT as e) -> VarArgGADT (a ': as) e

-- | Uncurried representation: in this representation Functor, Applicative, Monad instances are already given by (->)
type VarArgUncurried args e = VarArgTuple args -> e

-- | Tuple of the arguments, a heterogeneous list (known as HList)
data VarArgTuple (args :: [*]) where
  DictNil :: VarArgTuple '[]
  DictCons :: a -> VarArgTuple as -> VarArgTuple (a ': as)

-- * Isomorphisms between the representations

class ArgList (args :: [*]) where
  varArgGADT  :: Lens.Iso (VarArg args e) (VarArg args e') (VarArgGADT args e) (VarArgGADT args e')
  varArgGADTCurry :: Lens.Iso (VarArgUncurried args e) (VarArgUncurried args e') (VarArgGADT args e) (VarArgGADT args e')

instance ArgList '[] where
  varArgGADT  = iso DNil (\(DNil x) -> x)
  varArgGADTCurry = iso (\f -> DNil $ f DictNil) (\(DNil e) DictNil -> e)

instance ArgList as => ArgList (a ': as) where
  varArgGADT  = iso (\f -> DCons ((^. varArgGADT) . f))
                    (\(DCons f) -> ((^. Lens.from varArgGADT) . f))
  varArgGADTCurry = iso (\f -> DCons $ \a -> (f . DictCons a) ^. varArgGADTCurry)
                        (\(DCons f) (DictCons a as) -> (f a ^. Lens.from varArgGADTCurry) as)

varArgCurry :: ArgList args => Lens.Iso (VarArgUncurried args e) (VarArgUncurried args e') (VarArg args e) (VarArg args e')
varArgCurry = varArgGADTCurry . Lens.from varArgGADT

{-
instance Functor (VarArgGADT args) where
  fmap f (DNil a)  = DNil (f a)
  fmap f (DCons g) = DCons (fmap f . g)

-- | see https://stackoverflow.com/questions/45178068/fmap-over-variable-argument-function/
mapVarArg :: forall args e e' . ArgList args => (e -> e') -> VarArg args e -> VarArg args e'
mapVarArg f = Lens.under (Lens.from (varArgGADT @args)) (fmap f)
-}


-- * views with persisent state

-- | like 'mkStatefulView', but the component remembers its state when it is re-rendered
mkPersistentStatefulView
    :: forall (state :: *) (args :: [*]) elemM elemM'.
       (Typeable state, Typeable state, Eq state
       , ViewProps args ('StatefulEventHandlerCode (Int, LocalStateRef state))
       , Typeable args, AllEq args
       , ArgList args

       -- FIXME (react-hs):
       -- define
       --   type ViewPropsToElement args code = VarArg args (ReactElementM code)
       -- so the next lines in context will not be needed
       , ViewPropsToElement args ('StatefulEventHandlerCode state)
         ~ VarArg args elemM
       , elemM ~ ReactElementM ('StatefulEventHandlerCode state) ()
       , ViewPropsToElement args ('StatefulEventHandlerCode (Int, LocalStateRef state))
         ~ VarArg args elemM'
       , elemM' ~ ReactElementM ('StatefulEventHandlerCode (Int, LocalStateRef state)) ()
       )
    => JSString -- ^ A name for this view, used only for debugging/console logging
    -> LocalStateRef state -- ^ The reference of the initial state
    -> Maybe (state -> VarArg args state)  -- ^ Local state update function, applied if args change
    -> (state -> ViewPropsToElement args ('StatefulEventHandlerCode state))
    -> View args

{-# NOINLINE mkPersistentStatefulView #-}
mkPersistentStatefulView name rst_ upd trans
  = mkStatefulView name (0 :: Int, rst_) mtrans
  where
    mtrans (i, rst) = i `seq` (res ^. varArgCurry @args)
      where
        res :: VarArgTuple args -> elemM'
        res ps = unsafePerformIO $ do
          let r = rst ^. unLocalStateRef . unNoJSONRep
          st <- readIORef r
          let st' = maybe (pure st) (\updf -> updf st ^. Lens.from varArgCurry) upd ps
          writeIORef r st'
          pure . transHandler transformEventHandler $ (trans st' ^. Lens.from varArgCurry) ps

{-# NOINLINE transformEventHandler #-}
transformEventHandler :: StatefulViewEventHandler state -> StatefulViewEventHandler (Int, LocalStateRef state)
transformEventHandler handler (i, rst) = unsafePerformIO $ do
  let r = rst ^. unLocalStateRef . unNoJSONRep
  st <- readIORef r
  let (actions, mst) = handler st
  case mst of
    Nothing -> pure (actions, Nothing)
    Just st' -> do
      writeIORef r st'
      pure (actions, Just (i+1, rst))
