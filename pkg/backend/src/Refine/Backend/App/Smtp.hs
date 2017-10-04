{-# LANGUAGE CPP #-}
#include "language_backend.hs"

module Refine.Backend.App.Smtp
  ( MonadSmtp(sendMailTo)
  , IsEmailRecipient(recipientToAddresses)
  , IsEmailMessage(renderEmail)
  , EmailMessage(..)
  , checkSendMail

    -- re-exports
  , Address(..)
  ) where
#include "import_backend.hs"

import qualified Data.ByteString as SBS
import           Network.Mail.Mime (Address(Address), sendmailCustomCaptureOutput, simpleMail', renderMail)
import           System.Random
import qualified Web.Users.Types as Users

import           Refine.Backend.Config
import           Refine.Backend.App.Core
import           Refine.Common.Types


-- * types and classes

class MonadSmtp app where
  sendMailTo :: (IsEmailMessage msg, Show msg) => msg -> app ()

class IsEmailRecipient r where
  recipientToAddresses :: r -> [Address]

class IsEmailMessage msg where
  renderEmail :: SmtpCfg -> msg -> RandomGen g => g -> ([LBS], g)


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

instance IsEmailRecipient Users.User where
  recipientToAddresses u = [Address (Just $ Users.u_name u) (Users.u_email u)]

instance IsEmailRecipient Address where
  recipientToAddresses = (:[])

instance IsEmailRecipient r => IsEmailRecipient [r] where
  recipientToAddresses = mconcat . fmap recipientToAddresses


-- * message rendering

instance IsEmailRecipient r => IsEmailMessage (EmailMessage r) where
  renderEmail cfg msg g = foldl' aggr ([], g) recipients
    where
      aggr :: RandomGen g => ([LBS], g) -> Address -> ([LBS], g)
      aggr (dones, g') adr = case renderMail g' (mail adr) of (done, g'') -> (done: dones, g'')

      recipients = recipientToAddresses $ msg ^. emailRecipient
      sender     = Address (Just $ cfg ^. smtpSenderName) (cfg ^. smtpSenderEmail)
      subj       = msg ^. emailSubject
      mail rcp   = simpleMail' rcp sender subj (cs $ msg ^. emailBody)


-- * message sending

instance MonadSmtp (AppM db) where
  sendMailTo = sendMailToAppM

sendMailToAppM :: (IsEmailMessage msg, Show msg, m ~ AppM db) => msg -> m ()
sendMailToAppM msg = do
  mcfg <- asks . view $ appConfig . cfgSmtp
  case mcfg of
    Nothing -> do
      appLog LogInfo $ "sendMailTo: no config, email dropped: " <> show msg
    Just (cfg :: SmtpCfg) -> do
      appLog LogInfo $ "sendMailTo: " <> show (cfg, msg)
      msgslbs :: [LBS] <- liftIO . getStdRandom $ renderEmail cfg msg

      forM_ msgslbs $ \msglbs -> do
       (liftIO . try $ sendmailCustomCaptureOutput (cs $ cfg ^. smtpSendmailPath) (cs <$> cfg ^. smtpSendmailArgs) msglbs)
        >>= \case
          Right (out, err) -> do
            do
              unless (SBS.null out) . appLog LogWarning $ "sendmail produced output on stdout: " <> cs out
              unless (SBS.null err) . appLog LogWarning $ "sendmail produced output on stderr: " <> cs err
          Left e ->
            throwError . AppSmtpError . SmtpError $ e


-- * test mail system

-- | Run sendMail to check that we can send emails. Throw an error if sendmail is not available,
-- doesn't work, or makes unexpected noise on stdout, stderr.
checkSendMail :: Config -> IO ()
checkSendMail cfg = do
  case cfg ^. cfgSmtp of
    Nothing -> do
      -- log "smtp: off."
      pure ()
    Just scfg -> do
      -- log $ "smtp: " <> show scfg
      let msg = EmailMessage
            { _emailRecipient = Address Nothing (scfg ^. smtpDefaultRecipient)
            , _emailSubject   = "[starting re-fine server]"
            , _emailBody      = cs $ explainConfig cfg "active server configuration" False
            }
      [msglbs :: LBS] <- liftIO . getStdRandom $ renderEmail scfg msg
      result :: Either SomeException (SBS, SBS)
        <- try $ sendmailCustomCaptureOutput (cs $ scfg ^. smtpSendmailPath) (cs <$> scfg ^. smtpSendmailArgs) msglbs
      case result of
        Right ("", "") -> pure ()
        bad -> throwIO . ErrorCall $ "smtp failed: " <> show bad
