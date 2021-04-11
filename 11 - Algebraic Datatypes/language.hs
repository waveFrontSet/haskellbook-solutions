module Language where
import           Data.Char

capitalizeWord :: String -> String
capitalizeWord ""       = ""
capitalizeWord (x : xs) = (toUpper x : xs)

capitalizeParagraph :: String -> String
capitalizeParagraph = go . capitalizeWord
 where
  go :: String -> String
  go ""               = ""
  go ('.' : ' ' : xs) = ('.' : ' ' : go (capitalizeWord xs))
  go (x         : xs) = (x : go xs)
