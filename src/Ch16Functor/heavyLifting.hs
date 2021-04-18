module HeavyLifting where

import           Test.Hspec

-- 1.
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = fmap (* 2) (\x -> x - 2)

-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi     = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
  in  fmap (* 3) changed


main :: IO ()
main = hspec $ do
  describe "First exercise" $ do
    it "returns [2]" $ do
      a `shouldBe` [2]
  describe "Second exercise" $ do
    it "returns Just ..." $ do
      b `shouldBe` (Just ["Hi,lol", "Hellolol"])
  describe "Third exercise" $ do
    it "returns -2" $ do
      c 1 `shouldBe` (-2)
  describe "Fourth exercise" $ do
    it "returns '1[0,1,2,3]'" $ do
      d 0 `shouldBe` "1[0,1,2,3]"
  describe "Fifth exercise" $ do
    it "returns 3693" $ do
      e `shouldReturn` 3693
