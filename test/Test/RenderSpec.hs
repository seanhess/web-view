module Test.RenderSpec (spec) where

import Test.Syd
import Web.View
import Web.View.Style


spec :: Spec
spec = do
  describe "render" $ do
    it "should match expected output" $ do
      let res = renderText $ el bold "hi"
      pureGoldenTextFile "test/resources/basic.txt" res
    it "should escape properly" $ do
      let res = renderText $ do
            el (att "title" "I have some apos' and quotes \" and I'm a <<great>> attribute!!!") "I am <malicious> &apos;user"
            el (att "title" "I have some apos' and quotes \" and I'm a <<great>> attribute!!!") $ do
              text "I am <malicious> &apos;user"
              text "I am another <malicious> &apos;user"
      pureGoldenTextFile "test/resources/escaping.txt" res
    it "should escape properly" $ do
      let res = renderText $ el bold $ raw "<svg>&\"'</svg>"
      pureGoldenTextFile "test/resources/raw.txt" res
    it "should skip css when no css attributes" $ do
      let res = renderText $ el (addClass $ cls "empty") "hi"
      pureGoldenTextFile "test/resources/nocss.txt" res
