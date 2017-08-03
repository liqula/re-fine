{-# LANGUAGE RankNTypes                 #-}

-- | CAVEAT: https://ghc.haskell.org/trac/ghc/ticket/9562
module Refine.Backend.App.Smtp where

import Refine.Backend.Prelude
import System.Random
import Refine.Backend.Config

class MonadSmtp app where
  sendMailTo :: (IsEmailMessage msg, Show msg) => msg -> app ()

class IsEmailMessage msg where
  renderEmail :: SmtpCfg -> msg -> RandomGen g => g -> (LBS, g)
