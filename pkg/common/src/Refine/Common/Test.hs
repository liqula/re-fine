{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Refine.Common.Test (module P) where

import Test.Hspec as P
import Test.QuickCheck as P
import Test.QuickCheck.Instances as P ()
import Test.QuickCheck.Monadic as P

import Refine.Common.Test.Arbitrary as P
import Refine.Common.Test.HttpApiData as P
import Refine.Common.Test.Samples as P
