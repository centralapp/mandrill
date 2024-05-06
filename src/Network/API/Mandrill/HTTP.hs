{-# LANGUAGE OverloadedStrings #-}
module Network.API.Mandrill.HTTP where

import           Data.Aeson
import qualified Data.Text                     as T
import           Network.API.Mandrill.Settings
import           Network.API.Mandrill.Types
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types

toMandrillResponse :: (MandrillEndpoint ep, FromJSON a, ToJSON rq)
                   => ep
                   -> rq
                   -> Maybe Manager
                   -> IO (MandrillResponse a)
toMandrillResponse = toMandrillResponseMod id

{- | This function allows us to modify the 'Request' after we construct it from the url and JSON body.

Note that the function is applied _after_ the request is constructed, so be careful about modifications
that change existing parameters. E.g. it's better to _prepend_ to the list of headers than to override them.
-}
toMandrillResponseMod :: (MandrillEndpoint ep, FromJSON a, ToJSON rq)
                      => (Request -> Request)
                      -> ep
                      -> rq
                      -> Maybe Manager
                      -> IO (MandrillResponse a)
toMandrillResponseMod modR ep rq mbMgr = do

  let fullUrl = mandrillUrl <> toUrl ep
      headers = [(hContentType, "application/json")]
      jsonBody = encode rq

  rq' <- parseRequest (T.unpack fullUrl)

  let req = modR (rq' {
        method = "POST"
      , requestHeaders = headers
      , requestBody = RequestBodyLBS jsonBody
      })

  mgr <- maybe (newManager tlsManagerSettings) return mbMgr
  res <- responseBody <$> httpLbs req mgr
  case eitherDecode res of
    Left e  ->  fail e
    Right v -> return v
