{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import ExampleWAIServer (handler)

spec :: Spec
spec = with (return handler) $
  describe "GET /" $ do
    it "responds 200" $
      get "/" `shouldRespondWith` 200
    it "serves JSON" $
      get "/" `shouldRespondWith`
        [json|{service: "example-wai-server", version: "0.0.0"}|]

main :: IO ()
main = hspec spec
