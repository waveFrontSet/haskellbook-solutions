module FixerUpper where

-- 1.
x = const <$> Just <$> "Hello" <*> "World"

-- 2.
y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
