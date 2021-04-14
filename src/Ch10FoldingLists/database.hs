-- p. 371
module Database where

import           Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (unpackDates) []
 where
  unpackDates (DbDate dt) acc = dt : acc
  unpackDates _           acc = acc

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (unpackInts) []
 where
  unpackInts (DbNumber x) acc = x : acc
  unpackInts _            acc = acc

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb xs = genericSum xs / genericLength xs
 where
  genericSum    = fromIntegral . sumDb
  genericLength = fromIntegral . length . filterDbNumber
