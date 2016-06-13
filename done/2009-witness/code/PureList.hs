
module PureList where

data List a
      = Nil
      | Cons a (List a)

myMap f list
 = case list of
    Nil  -> Nil
    Cons x xs -> Cons (f x) (myMap f xs)
