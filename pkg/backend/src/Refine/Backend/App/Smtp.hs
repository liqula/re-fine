{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
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
{-# LANGUAGE QuasiQuotes                #-}
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

module Refine.Backend.App.Smtp
  ( MonadSmtp(sendMailTo)
  , IsEmailRecipient(recipientToAddresses)
  , IsEmailMessage(renderEmail)
  , EmailMessage(..)
  , checkSendMail
  ) where

import Refine.Backend.Prelude

import qualified Data.ByteString as SBS
import           Network.Mail.Mime (Address(Address), sendmailCustomCaptureOutput, simpleMail', renderMail)
import           System.Random

import           Refine.Backend.Config
import           Refine.Backend.App.Core
import           Refine.Common.Types


-- * types and classes

class MonadSmtp app where
  sendMailTo :: (IsEmailMessage msg, Show msg) => msg -> app ()

class IsEmailRecipient r where
  recipientToAddresses :: r -> [Address]

class IsEmailMessage msg where
  renderEmail :: SmtpCfg -> msg -> RandomGen g => g -> (LBS, g)


data EmailMessage r = EmailMessage
  { _emailRecipient :: r
  , _emailSubject   :: ST
  , _emailBody      :: ST
  }
  deriving (Eq, Ord, Show, Read, Generic)

makeLenses ''EmailMessage


-- * recipients

instance IsEmailRecipient User where
  recipientToAddresses u = [Address (Just $ u ^. userName) (u ^. userEmail)]

instance IsEmailRecipient Address where
  recipientToAddresses = (:[])

instance IsEmailRecipient r => IsEmailRecipient [r] where
  recipientToAddresses = mconcat . fmap recipientToAddresses


-- * message rendering

instance IsEmailRecipient r => IsEmailMessage (EmailMessage r) where
  renderEmail cfg msg g = renderMail g mail
    where
      sender     = Address (Just $ cfg ^. smtpSenderName) (cfg ^. smtpSenderEmail)
      recipients = recipientToAddresses $ msg ^. emailRecipient
      subj       = msg ^. emailSubject
      mail       = simpleMail' (head recipients) sender subj (cs $ msg ^. emailBody)


-- * message sending

instance MonadSmtp (AppM db) where
  sendMailTo = sendMailToAppM

sendMailToAppM :: (IsEmailMessage msg, Show msg, m ~ AppM db) => msg -> m ()
sendMailToAppM msg = do
  mcfg <- asks . view $ appConfig . cfgSmtp
  case mcfg of
    Nothing -> pure ()
    Just (cfg :: SmtpCfg) -> do
      appLog $ "sendMailTo: " <> show (cfg, msg)
      msglbs :: LBS <- liftIO . getStdRandom $ renderEmail cfg msg

      (liftIO . try $ sendmailCustomCaptureOutput (cs $ cfg ^. smtpSendmailPath) (cs <$> cfg ^. smtpSendmailArgs) msglbs)
        >>= \case
          Right (out, err) -> do
            do
              unless (SBS.null out) . appLog $ "sendmail produced output on stdout: " <> cs out
              unless (SBS.null err) . appLog $ "sendmail produced output on stderr: " <> cs err
          Left e ->
            throwError . AppSmtpError . SmtpError $ e


-- * test mail system

-- | Run sendMail to check that we can send emails. Throw an error if sendmail
-- is not available or doesn't work.
checkSendMail :: AppM db ()
checkSendMail = do
  cfg <- asks . view $ appConfig
  case cfg ^. cfgSmtp of
    Nothing -> do
      appLog "smtp: off."
    Just scfg -> do
      appLog $ "smtp: " <> show scfg
      sendMailTo EmailMessage
        { _emailRecipient = Address Nothing (scfg ^. smtpDefaultRecipient)
        , _emailSubject   = "[starting re-fine server]"
        , _emailBody      = cs $ encode cfg
        }
