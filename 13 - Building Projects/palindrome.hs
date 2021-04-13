import           Control.Monad
import           Data.Char                      ( toLower )
import           System.Exit                    ( exitSuccess )

filterChars :: [Char]
filterChars = " ',.?!"
isPalindrome :: String -> Bool
isPalindrome s = cleanedString == reverse cleanedString
  where cleanedString = map toLower $ filter (`elem` filterChars) s


palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (isPalindrome line1) of
    True  -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
