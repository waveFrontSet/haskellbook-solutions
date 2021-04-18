module Ch15MonoidSemigroup.Optional where

import           Data.Monoid
import           Test.Hspec

data Optional a = Nada | Only a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada     only     = only
  (<>) only     Nada     = only
  (<>) (Only x) (Only y) = Only $ x <> y

instance Monoid a => Monoid (Optional a) where
  mempty  = Nada
  mappend = (<>)



main :: IO ()
main = hspec $ do
  describe "Mappend" $ do
    it "works with sum" $ do
      let onlySum = Only (Sum (1 :: Integer))
      mappend onlySum onlySum `shouldBe` Only (Sum 2)
    it "works with product" $ do
      let onlyFour = Only (Product (4 :: Integer))
          onlyTwo  = Only (Product 2)
      mappend onlyFour onlyTwo `shouldBe` Only (Product 8)
    it "has Nada as neutral element" $ do
      let onlyOne  = Only (Sum (1 :: Integer))
          onlyList = Only [1 :: Integer]
      onlyOne `mappend` Nada `shouldBe` onlyOne
      onlyList `mappend` Nada `shouldBe` onlyList
      Nada `mappend` onlyOne `shouldBe` onlyOne
      Nada `mappend` onlyList `shouldBe` onlyList
