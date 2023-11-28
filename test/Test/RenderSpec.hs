module Test.RenderSpec (spec) where

import Test.Syd
import Web.View


spec :: Spec
spec = do
  describe "render" $ do
    it "should match expected output" $ do
      let res = renderText $ el bold "hi"
      pureGoldenTextFile "test/resources/basic.txt" res
