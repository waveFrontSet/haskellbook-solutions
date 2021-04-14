module MultTestSpec where


import           Ch08Recursion.ChapterExercises ( recurseMult )
import           Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Multiplication" $ do
    it "correctly multiplies by 0 on the right" $ do
      recurseMult 2 0 `shouldBe` 0
    it "correctly multiplies by 0 on the left" $ do
      recurseMult 0 2 `shouldBe` 0
    it "correctly multiplies by 1 on the left" $ do
      recurseMult 1 2 `shouldBe` 2
    it "correctly multiplies by 1 on the right" $ do
      recurseMult 2 1 `shouldBe` 2
    it "correctly multiplies other numbers" $ do
      recurseMult 5 4 `shouldBe` 20
