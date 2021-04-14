module MkPerson where

type Name = String
type Age = Integer

data Person = Person Name Age
  deriving Show

data PersonInvalid =
  NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0
  = Right $ Person name age
  | name == ""
  = Left NameEmpty
  | not (age > 0)
  = Left AgeTooLow
  | otherwise
  = Left
    $  PersonInvalidUnknown
    $  "Name was: "
    ++ show name
    ++ " Age was: "
    ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Type a name:"
  name <- getLine
  putStrLn "Type an age:"
  ageStr <- getLine
  let age :: Integer
      age = read ageStr
  case mkPerson name age of
    Left err -> do
      putStrLn ("An error occurred: " ++ show err)
    Right person -> do
      putStrLn ("Yay! Successfully got a person: " ++ show person)
