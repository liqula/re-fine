{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | (This module is necessary due to GHC/TH stage restrictions.)
module Refine.Prelude.BuildInfo.TH
where

import Data.Data (Data)
import Language.Haskell.TH (Q, Exp, runIO)
import Language.Haskell.TH.Quote (dataToExpQ)
import System.Process (readProcess)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)


gitCommitHashIO :: IO String
gitCommitHashIO = take 8 <$> readProcess "git" ["rev-parse", "HEAD"] ""

gitBuildTimestampIO :: IO String
gitBuildTimestampIO = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" <$> getCurrentTime

runAtbuildTime :: Data a => IO a -> Q Exp
runAtbuildTime action = runIO action >>= dataToExpQ (const Nothing)
