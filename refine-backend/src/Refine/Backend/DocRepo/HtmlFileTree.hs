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

module Refine.Backend.DocRepo.HtmlFileTree where

import Refine.Common.Types.VDoc


-- | Take a file path 'fp' and a canonicalized vdoc version, create a directory under 'fp', unravel
-- the vdoc version into that directory and return its filepath.
htmlToFileTree :: FilePath -> VDocVersion 'HTMLCanonical -> IO FilePath
htmlToFileTree = undefined

-- | Take the file path of an unravelled vdoc version, re-ravel it and return it.
htmlFromFileTree :: FilePath -> IO (VDocVersion 'HTMLCanonical)
htmlFromFileTree = undefined
