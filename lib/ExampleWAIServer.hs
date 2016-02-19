{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExampleWAIServer (handler) where

import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Types (status200)
import Network.Wai

data Meta = Meta {
    service :: Text
  , version :: Text
} deriving (Generic, Show)

instance ToJSON Meta

handler :: Application
handler _ respond =
    respond $ responseLBS
        status200
        [("Content-Type", "application/json")]
        (encode Meta {
            service="example-wai-server"
          , version="0.0.0"})
