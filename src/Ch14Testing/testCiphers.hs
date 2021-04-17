module TestCiphers where

import           Ch09Lists.Cipher               ( caesar
                                                , unCaesar
                                                )
import           Ch11AlgebraicDatatypes.Vigenere
                                                ( unvigenere
                                                , vigenere
                                                )
import           Test.QuickCheck


regularChars :: [Char]
regularChars = ['a' .. 'z']

regularChar :: Gen Char
regularChar = elements regularChars

regularKeyword :: Gen String
regularKeyword = listOf regularChar

regularTextChars :: Gen Char
regularTextChars = elements (' ' : regularChars)

regularText :: Gen String
regularText = listOf regularTextChars

caesarInput :: Gen (Int, String)
caesarInput = do
  i    <- arbitrary
  text <- regularText
  return (i, text)

vigenereInput :: Gen (String, String)
vigenereInput = do
  keyword <- regularKeyword
  text    <- regularText
  return (keyword, text)

areInverses :: Eq b => (a -> b) -> (b -> a) -> b -> Bool
areInverses f g x = (f . g) x == x

prop_caesarIsInverseToUncaesar :: Property
prop_caesarIsInverseToUncaesar = forAll
  caesarInput
  (\(i, text) ->
    let c  = caesar i
        uc = unCaesar i
    in  areInverses c uc text && areInverses uc c text
  )

prop_vigenereIsInverseToUnvigenere :: Property
prop_vigenereIsInverseToUnvigenere = forAll
  vigenereInput
  (\(keyword, text) ->
    let v  = vigenere keyword
        uv = unvigenere keyword
    in  areInverses v uv text && areInverses uv v text
  )

main :: IO ()
main = do
  quickCheck prop_caesarIsInverseToUncaesar
  quickCheck prop_vigenereIsInverseToUnvigenere
