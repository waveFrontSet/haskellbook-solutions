module SplitString where

splitUsing :: Char -> String -> [String]
splitUsing _ "" = []
splitUsing c s  = first : splitUsing c remainder
 where
  first     = takeWhile (/= c) s
  remainder = dropWhile (== c) $ dropWhile (/= c) s


myWords :: String -> [String]
myWords = splitUsing ' '


myLines :: String -> [String]
myLines = splitUsing '\n'
