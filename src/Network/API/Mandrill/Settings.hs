{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.Settings
  ( MandrillCalls(..)
  , mandrillUrl
  , MandrillEndpoint(..)
  )
where

import qualified Data.Text                     as T

mandrillUrl :: T.Text
mandrillUrl = "https://mandrillapp.com/api/1.0/"

data MandrillCalls =
  -- Users API
    UsersInfo
  | UsersPing
  | UsersPing2
  | UsersSenders
  -- Messages API
  | MessagesSend
  | MessagesSendTemplate
  | MessagesSearch
  -- Inbound API
  | RoutesAdd
  | DomainsAdd
  -- Senders API
  | VerifyDomain
  | GetTemplates

  deriving Show

class MandrillEndpoint ep where
  toUrl :: ep -> T.Text

instance MandrillEndpoint MandrillCalls where
  toUrl UsersInfo            = "users/info.json"
  toUrl UsersPing            = "users/ping.json"
  toUrl UsersPing2           = "users/ping2.json"
  toUrl UsersSenders         = "users/senders.json"
  toUrl MessagesSend         = "messages/send.json"
  toUrl MessagesSendTemplate = "messages/send-template.json"
  toUrl MessagesSearch       = "messages/search.json"
  toUrl DomainsAdd           = "inbound/add-domain.json"
  toUrl RoutesAdd            = "inbound/add-route.json"
  toUrl VerifyDomain         = "senders/verify-domain.json"
  toUrl GetTemplates         = "templates/list.json"
