{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Brig.User.Template
    ( UserTemplates              (..)
    , ActivationSmsTemplate      (..)
    , ActivationEmailTemplate    (..)
    , ActivationCallTemplate     (..)
    , PasswordResetSmsTemplate   (..)
    , PasswordResetEmailTemplate (..)
    , InvitationEmailTemplate    (..)
    , InvitationSmsTemplate      (..)
    , LoginSmsTemplate           (..)
    , LoginCallTemplate          (..)
    , DeletionSmsTemplate        (..)
    , DeletionEmailTemplate      (..)
    , NewClientEmailTemplate     (..)
    , loadUserTemplates

      -- * Re-exports
    , Template
    , renderText
    , renderHtml
    ) where

import Brig.Template
import Brig.Types
import Data.Monoid
import Data.Text (Text)

import qualified Brig.Options       as Opts
import qualified Data.Text.Encoding as Text

data UserTemplates = UserTemplates
    { activationSms         :: !ActivationSmsTemplate
    , activationCall        :: !ActivationCallTemplate
    , activationEmail       :: !ActivationEmailTemplate
    , activationEmailUpdate :: !ActivationEmailTemplate
    , passwordResetSms      :: !PasswordResetSmsTemplate
    , passwordResetEmail    :: !PasswordResetEmailTemplate
    , invitationEmail       :: !InvitationEmailTemplate
    , invitationSms         :: !InvitationSmsTemplate
    , loginSms              :: !LoginSmsTemplate
    , loginCall             :: !LoginCallTemplate
    , deletionSms           :: !DeletionSmsTemplate
    , deletionEmail         :: !DeletionEmailTemplate
    , newClientEmail        :: !NewClientEmailTemplate
    }

data ActivationSmsTemplate = ActivationSmsTemplate
    { activationSmslUrl   :: !Template
    , activationSmsText   :: !Template
    , activationSmsSender :: !Text
    }

data ActivationCallTemplate = ActivationCallTemplate
    { activationCallText  :: !Template
    }

data ActivationEmailTemplate = ActivationEmailTemplate
    { activationEmailUrl        :: !Template
    , activationEmailSubject    :: !Template
    , activationEmailBodyText   :: !Template
    , activationEmailBodyHtml   :: !Template
    , activationEmailSender     :: !Email
    , activationEmailSenderName :: !Text
    }

data DeletionEmailTemplate = DeletionEmailTemplate
    { deletionEmailUrl        :: !Template
    , deletionEmailSubject    :: !Template
    , deletionEmailBodyText   :: !Template
    , deletionEmailBodyHtml   :: !Template
    , deletionEmailSender     :: !Email
    , deletionEmailSenderName :: !Text
    }

data InvitationEmailTemplate = InvitationEmailTemplate
    { invitationEmailUrl        :: !Template
    , invitationEmailSubject    :: !Template
    , invitationEmailBodyText   :: !Template
    , invitationEmailBodyHtml   :: !Template
    , invitationEmailSender     :: !Email
    , invitationEmailSenderName :: !Text
    }

data InvitationSmsTemplate = InvitationSmsTemplate
    { invitationSmsUrl       :: !Template
    , invitationSmsText      :: !Template
    , invitationSmsSender    :: !Text
    }

data PasswordResetEmailTemplate = PasswordResetEmailTemplate
    { passwordResetEmailUrl        :: !Template
    , passwordResetEmailSubject    :: !Template
    , passwordResetEmailBodyText   :: !Template
    , passwordResetEmailBodyHtml   :: !Template
    , passwordResetEmailSender     :: !Email
    , passwordResetEmailSenderName :: !Text
    }

data PasswordResetSmsTemplate = PasswordResetSmsTemplate
    { passwordResetSmsText   :: !Template
    , passwordResetSmsSender :: !Text
    }

data LoginSmsTemplate = LoginSmsTemplate
    { loginSmsUrl    :: !Template
    , loginSmsText   :: !Template
    , loginSmsSender :: !Text
    }

data LoginCallTemplate = LoginCallTemplate
    { loginCallText   :: !Template
    }

data DeletionSmsTemplate = DeletionSmsTemplate
    { deletionSmsUrl    :: !Template
    , deletionSmsText   :: !Template
    , deletionSmsSender :: !Text
    }

data NewClientEmailTemplate = NewClientEmailTemplate
    { newClientEmailSubject    :: !Template
    , newClientEmailBodyText   :: !Template
    , newClientEmailBodyHtml   :: !Template
    , newClientEmailSender     :: !Email
    , newClientEmailSenderName :: !Text
    }

loadUserTemplates :: Opts.Opts -> IO (Localised UserTemplates)
loadUserTemplates o = readLocalesDir defLocale templateDir $ \fp ->
    UserTemplates
        <$> (ActivationSmsTemplate smsActivationUrl
                <$> readTemplate (fp <> "/sms/activation.txt")
                <*> pure smsSender)
        <*> (ActivationCallTemplate
                <$> readTemplate (fp <> "/call/activation.txt"))
        <*> (ActivationEmailTemplate activationUrl
                <$> readTemplate (fp <> "/email/activation-subject.txt")
                <*> readTemplate (fp <> "/email/activation.txt")
                <*> readTemplate (fp <> "/email/activation.html")
                <*> pure emailSender
                <*> readText (fp <> "/email/sender.txt"))
        <*> (ActivationEmailTemplate activationUrl
                <$> readTemplate (fp <> "/email/update-subject.txt")
                <*> readTemplate (fp <> "/email/update.txt")
                <*> readTemplate (fp <> "/email/update.html")
                <*> pure emailSender
                <*> readText (fp <> "/email/sender.txt"))
        <*> (PasswordResetSmsTemplate
                <$> readTemplate (fp <> "/sms/password-reset.txt")
                <*> pure smsSender)
        <*> (PasswordResetEmailTemplate passwordResetUrl
                <$> readTemplate (fp <> "/email/password-reset-subject.txt")
                <*> readTemplate (fp <> "/email/password-reset.txt")
                <*> readTemplate (fp <> "/email/password-reset.html")
                <*> pure emailSender
                <*> readText (fp <> "/email/sender.txt"))
        <*> (InvitationEmailTemplate invitationUrl
                <$> readTemplate (fp <> "/email/invitation-subject.txt")
                <*> readTemplate (fp <> "/email/invitation.txt")
                <*> readTemplate (fp <> "/email/invitation.html")
                <*> pure emailSender
                <*> readText (fp <> "/email/sender.txt"))
        <*> (InvitationSmsTemplate invitationUrl
                <$> readTemplate (fp <> "/sms/invitation.txt")
                <*> pure smsSender)
        <*> (LoginSmsTemplate smsActivationUrl
                <$> readTemplate (fp <> "/sms/login.txt")
                <*> pure smsSender)
        <*> (LoginCallTemplate
                <$> readTemplate (fp <> "/call/login.txt"))
        <*> (DeletionSmsTemplate deletionUserUrl
                <$> readTemplate (fp <> "/sms/deletion.txt")
                <*> pure smsSender)
        <*> (DeletionEmailTemplate deletionUserUrl
                <$> readTemplate (fp <> "/email/deletion-subject.txt")
                <*> readTemplate (fp <> "/email/deletion.txt")
                <*> readTemplate (fp <> "/email/deletion.html")
                <*> pure emailSender
                <*> readText (fp <> "/email/sender.txt"))
        <*> (NewClientEmailTemplate
                <$> readTemplate (fp <> "/email/new-client-subject.txt")
                <*> readTemplate (fp <> "/email/new-client.txt")
                <*> readTemplate (fp <> "/email/new-client.html")
                <*> pure emailSender
                <*> readText (fp <> "/email/sender.txt"))
  where
    gOptions         = Opts.general $ Opts.emailSMS o
    uOptions         = Opts.user $ Opts.emailSMS o
    emailSender      = Opts.emailSender gOptions
    smsSender        = Opts.smsSender gOptions
    smsActivationUrl = template $ Opts.smsActivationUrl uOptions
    activationUrl    = template $ Opts.activationUrl    uOptions
    passwordResetUrl = template $ Opts.passwordResetUrl uOptions
    invitationUrl    = template $ Opts.invitationUrl    uOptions
    deletionUserUrl  = template $ Opts.deletionUrl      uOptions

    defLocale = Opts.setDefaultLocale (Opts.optSettings o)
    templateDir = Opts.templateDir gOptions <> "/user"

