module MultTestSpec where


import           Test.Hspec

recurseMult :: (Integral a) => a -> a -> a
recurseMult 0 _ = 0
recurseMult n m = m + recurseMult (n - 1) m


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
