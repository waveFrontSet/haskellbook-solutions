module Phone where
import           Data.Char
import           Data.List

type Digit = Char
type Presses = Int
type Letters = [Char]
data DaPhone = NoOp | Button Digit Letters DaPhone

stdPhoneLayout = Button
  '1'
  "1"
  (Button
    '2'
    "abc2"
    (Button
      '3'
      "def3"
      (Button
        '4'
        "ghi4"
        (Button
          '5'
          "jkl5"
          (Button
            '6'
            "mno6"
            (Button
              '7'
              "pqrs7"
              (Button
                '8'
                "tuv8"
                (Button
                  '9'
                  "wxyz9"
                  (Button '0' " 0" (Button '*' "" (Button '#' ".," NoOp)))
                )
              )
            )
          )
        )
      )
    )
  )

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol OK. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "OK. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

findShiftKey :: DaPhone -> Digit
findShiftKey NoOp = '*'
findShiftKey (Button digit letters rest) | letters == "" = digit
                                         | otherwise     = findShiftKey rest

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps NoOp _ = []
reverseTaps phone@(Button digit letters rest) c
  | isUpper c        = (findShiftKey phone, 1) : reverseTaps phone (toLower c)
  | c `elem` letters = [(digit, index + 1)]
  | otherwise        = reverseTaps rest c
 where
  index = case c `elemIndex` letters of
    Just i  -> i
    Nothing -> 0

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . map (reverseTaps phone)

convertConversation :: DaPhone -> [String] -> [[(Digit, Presses)]]
convertConversation phone = map (cellPhonesDead phone)

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

-- 4.
mostPopularLetter :: Ord a => [a] -> a
mostPopularLetter =
  fst . maximumBy (\x y -> compare (snd x) (snd y)) . computeCounts
 where
  computeCounts :: Eq a => [a] -> [(a, Int)]
  computeCounts [] = []
  computeCounts xs@(x : rxs) =
    (x, length $ filter (== x) xs) : computeCounts (filter (/= x) rxs)

-- 5.
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopularLetter . words . concat
