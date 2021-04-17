module Generators where

import           Test.QuickCheck                ( Gen
                                                , elements
                                                , frequency
                                                )

data Fool = Fulse | Frue deriving (Eq, Show)
-- 1.
foolGen :: Gen Fool
foolGen = elements [Fulse, Frue]

-- 2.
imbalancedFoolGen :: Gen Fool
imbalancedFoolGen = frequency [(2, return Fulse), (1, return Frue)]
