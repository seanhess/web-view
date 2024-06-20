module Test.RenderSpec (spec) where

import Data.Text (Text)
import Test.Syd
import Web.View
import Web.View.Style
import Web.View.Types (Element (..))
import Web.View.View (tag')
import Prelude hiding (span)


spec :: Spec
spec = do
  describe "render" $ do
    describe "output" $ do
      it "should render simple output" $ do
        renderText (el_ "hi") `shouldBe` "<div>hi</div>"

    it "should match expected output" $ do
      goldenFile "test/resources/basic.txt" $ do
        pure $ renderText $ col (pad 10) $ el bold "hello" >> el_ "world"

  describe "escape" $ do
    it "should escape properly" $ do
      goldenFile "test/resources/escaping.txt" $ do
        pure $ renderText $ do
          el (att "title" "I have some apos' and quotes \" and I'm a <<great>> attribute!!!") "I am <malicious> &apos;user"
          el (att "title" "I have some apos' and quotes \" and I'm a <<great>> attribute!!!") $ do
            text "I am <malicious> &apos;user"
            text "I am another <malicious> &apos;user"

    it "should escape properly" $ do
      goldenFile "test/resources/raw.txt" $ do
        pure $ renderText $ el bold $ raw "<svg>&\"'</svg>"

  describe "empty rules" $ do
    it "should skip css class when no css attributes" $ do
      goldenFile "test/resources/nocssattrs.txt" $ do
        pure $ renderText $ do
          el (addClass $ cls "empty") "i have no css"
          el bold "i have some css"

    it "should skip css element when no css rules" $ do
      let res = renderText $ el (addClass $ cls "empty") "i have no css"
      res `shouldBe` "<div class='empty'>i have no css</div>"

  describe "inline" $ do
    it "should render text and inline elements inline" $ do
      let span = tag' (Element True) "span" :: Mod -> View c () -> View c ()
      let res =
            renderText $ do
              text "foo "
              span id "/"
              text " bar"
      res `shouldBe` "foo / bar"


goldenFile :: FilePath -> IO Text -> GoldenTest Text
goldenFile fp txt = do
  goldenTextFile fp $ do
    t <- txt
    pure $ t <> "\n"
