module ShortExercise where
import           Data.Char

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = fmap rev cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  x <- cap
  y <- rev
  return (x, y)

tupledM :: [Char] -> ([Char], [Char])
tupledM = cap >>= \x -> rev >>= \y -> return (x, y)
