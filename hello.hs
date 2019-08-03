module Hello where

  hello :: [Char] -> [Char]
  hello name = "Hello, " ++ name

  add3Numbers :: Num a => a -> (a -> (a -> a))
  add3Numbers x y z = x + y + z

  data FirstLast =
    FirstLast String String
    deriving (Show)

  data DOB =
    DOB String
    deriving (Show)

  data Person =
    Person FirstLast DOB
    deriving (Show)

  data FirstLastRec =
    FirstLastRec { first :: String
                 , last :: String }
                 deriving (Show)

  data DOBRec =
    DOBRec { date :: String }
    deriving (Show)

  data PersonRec =
    PersonRec { name :: FirstLastRec
              , birthday :: DOBRec }
              deriving (Show)

  data Jesse = Jesse

  type Name = String

  type Goats = Int

  newtype GoatsNew = GoatsNew Int

  newtype CowsNew = CowsNew Int

  data Foo = Bar
           | Baz
           deriving (Show)

  data WhatIsThis = String
                  | Integer
                  deriving (Show)

  data Perhaps a = Nope
                 | Yessir a
                 deriving (Show)

  data Listy a = Nil
               | Cons a (Listy a)
                 deriving (Eq)
  instance Show a => Show (Listy a) where
    show Nil = "[ ]"
    show (Cons a listy) = "[ " ++ show a ++ ", "++ show listy ++ " ]"

  toManyGoats :: GoatsNew -> Bool
  toManyGoats (GoatsNew n) = n > 42

  mappity :: Perhaps a -> (a -> b) -> Perhaps b
  mappity Nope _ = Nope
  mappity (Yessir a) f = Yessir (f a)

  addListy :: Num a => Listy a -> a
  addListy Nil = 0
  addListy (Cons n listy) = n + addListy listy

  main :: IO ()
  main = print $ hello "SDFP"
