-- p. 364, Exercise 6
module MapBool where
import           Data.Bool

mapBool :: (Eq a, Num a) => [a] -> [a]
mapBool = map (\x -> bool x (-x) (x == 3))
