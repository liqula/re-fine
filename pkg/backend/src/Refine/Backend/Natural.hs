{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Refine.Backend.Natural (cnToSn, natThrowError, (.), ($$)) where

import Control.Category ((.))
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Natural (($$))
import Data.Either (either)
import Prelude hiding ((.))

import qualified Control.Natural     as CN
import qualified Servant.Utils.Enter as SN


cnToSn :: (a CN.:~> b) -> (a SN.:~> b)
cnToSn (CN.Nat n) = SN.Nat n

natThrowError :: (Functor m, Show e) => ExceptT e m CN.:~> m
natThrowError = CN.Nat (fmap (either (error . show) id) . runExceptT)
