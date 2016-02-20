{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleWAIServer (handler) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Types (status200)
import Network.Wai

-- Metadata About the Server

data Meta = Meta {
    -- Name of the service
    service :: Text
    -- API version
  , version :: Text
} deriving (Generic, Show)

-- Make Meta JSON-serializable

instance ToJSON Meta

handler :: Application
handler _ respond =
    -- Respond to any request.
    -- The body of the response is a lazy bytestring (LBS).
    respond $ responseLBS
        status200
        [("Content-Type", "application/json")]
        (encode Meta {
            service="example-wai-server"
          , version="0.0.0"})
