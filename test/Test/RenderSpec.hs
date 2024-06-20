module Test.RenderSpec (spec) where

import Data.Text (Text)
import Test.Syd
import Web.View
import Web.View.Render (Line (..), LineEnd (..), renderLines)
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

      it "should render two elements" $ do
        renderText (el_ "hello" >> el_ "world") `shouldBe` "<div>hello</div>\n<div>world</div>"

      it "should match basic output with styles" $ do
        goldenFile "test/resources/basic.txt" $ do
          pure $ renderText $ col (pad 10) $ el bold "hello" >> el_ "world"

    describe "escape" $ do
      it "should escape properly" $ do
        goldenFile "test/resources/escaping.txt" $ do
          pure $ renderText $ do
            el (att "title" "I have some apos' and quotes \" and I'm a <<great>> attribute!!!") "I am <malicious> &apos;user"
            el (att "title" "I have some apos' and quotes \" and I'm a <<great>> attribute!!!") $ do
              el_ "I am <malicious> &apos;user"
              el_ "I am another <malicious> &apos;user"

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
      it "renderLines should respect inline text " $ do
        renderLines [Line Inline 0 "one ", Line Inline 0 "two"] `shouldBe` "one two"

      it "renderLines should respect inline tags " $ do
        renderLines [Line Inline 0 "one ", Line Inline 0 "two ", Line Inline 0 "<span>/</span>", Line Inline 0 " three"] `shouldBe` "one two <span>/</span> three"

      it "should render text and inline elements inline" $ do
        let span = tag' (Element True "span") :: Mod -> View c () -> View c ()
        let res =
              renderText $ do
                text "one "
                text "two "
                span id "/"
                text " three"
        res `shouldBe` "one two <span>/</span> three"

    describe "indentation" $ do
      it "should nested indent" $ do
        goldenFile "test/resources/nested.txt" $ do
          pure $ renderText $ do
            el_ $ do
              el_ $ do
                el_ "HI"


goldenFile :: FilePath -> IO Text -> GoldenTest Text
goldenFile fp txt = do
  goldenTextFile fp $ do
    t <- txt
    pure $ t <> "\n"
