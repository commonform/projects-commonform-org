-- Makes it easier to provide ByteStrings to WAI.
{-# LANGUAGE OverloadedStrings #-}

-- Makes it easier to set up JSON serialization with Aeson.
{-# LANGUAGE DeriveGeneric #-}

module ExampleWAIServer (handler) where

-- Imports for JSON with Aeson
import Data.Aeson
import Data.Text
import GHC.Generics

-- Imports for the WAI handler
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

-- Meta for This Service

-- Sure, you could just writ the corresponding JSON as a string, but
-- this gives us a chance to show aeson in action.

meta = Meta {
    service="example-wai-server"
  , version="0.0.0"
  }

-- A WAI request handler function.

handler :: Application
handler _ respond =
    -- Respond to any request.
    -- The body of the response is a lazy bytestring (LBS).
    respond $ responseLBS
        status200
        [("Content-Type", "application/json")]
        (encode meta)
