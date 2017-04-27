{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
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
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Orphans where

import           Data.Aeson
import           Data.String.Conversions (cs)
import           Data.Tree (Tree(..))
import           GHC.Generics
import           Text.HTML.Parser (Token(..), Attr(..))
import           Text.HTML.Tree (ParseTokenForestError(..), PStack (..))
import qualified Data.Text.Internal.Builder as Builder

import Refine.Prelude.TH (makeRefineType)


instance Ord a => Ord (Tree a) where
  compare (Node x xs) (Node x' xs') = compare (x, xs) (x', xs')


deriving instance Generic PStack
deriving instance Generic ParseTokenForestError
deriving instance Generic Attr

makeRefineType ''PStack
makeRefineType ''ParseTokenForestError
makeRefineType ''Attr

deriving instance FromJSON Token
deriving instance ToJSON Token


instance FromJSON Builder.Builder where
  parseJSON = withText "html comment" (pure . Builder.fromLazyText . cs)

instance ToJSON Builder.Builder where
  toJSON = toJSON . Builder.toLazyText
