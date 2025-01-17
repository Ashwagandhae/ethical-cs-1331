module Main
import Data.Fin
import Data.String

data LList : (a : Type) -> (n : Nat) -> Type where
  Nil : LList a Z
  Cons : a -> LList a n -> LList a (S n)

next : LList a (S n) -> Maybe (a, LList a n)
next (Cons x xs) = Just (x, xs)

addAt : LList a n -> Fin (S n) -> a -> LList a (S n)
addAt Nil FZ x = Cons x Nil
addAt (Cons y ys) FZ x = Cons x (Cons y ys)
addAt (Cons y ys) (FS i) x = Cons y (addAt ys i x)

removeAt : LList a (S n) -> Fin (S n) -> (a, LList a n)
removeAt (Cons x xs) FZ = (x, xs)
removeAt (Cons x (Cons y ys)) (FS i) = let
  (ret, xs) = removeAt (Cons y ys) i in (ret, Cons x xs)

removeEq : (Eq a) => LList a (S n) -> a -> Maybe (a, LList a n)
removeEq (Cons x Nil) t = if t == x then Just (x, Nil) else Nothing
removeEq (Cons x (xs @ (Cons _ _))) t =
  if t == x then Just (x, xs) else map (\(y, ys) => (y, Cons x ys)) (removeEq xs x)

removeStart : LList a (S n) -> (a, LList a n)
removeStart xs = removeAt xs 0

addEnd : LList a n -> a -> LList a (S n)
addEnd (Nil) x = Cons x Nil
addEnd (Cons y ys) x = Cons y $ addEnd ys x

get : {n : _} -> LList a n -> Fin n -> a
get {n=S _} xs i = fst $ removeAt xs i

getHead : LList a n -> Maybe a
getHead Nil = Nothing
getHead (Cons x _) = Just x

set : {n : _} -> LList a n -> Fin n -> a -> (a, LList a n)
set {n=S _} xs i x = let (ox, xs) = removeAt xs i in (ox, addAt xs i x)

contains : (Eq a) => LList a n -> a -> Bool
contains Nil _ = False
contains (Cons y ys) x = y == x || contains ys x

clear : LList a 0
clear = Nil

size : LList a n -> Nat
size Nil = 0
size (Cons _ xs) = 1 + size xs

isEmpty : LList a n -> Bool
isEmpty = (0 ==) . size

lListToList : LList a n -> List a
lListToList Nil = []
lListToList (Cons x xs) = x :: (lListToList xs)

listToLList : (xs : List a) -> LList a (length xs)
listToLList [] = Nil
listToLList (x :: xs) = Cons x (listToLList xs)

Show a => Show (LList a n) where
  show xs =
    "===== LINKEDLIST =====\nisEmpty: \{if isEmpty xs then "true" else "false"}" ++
    "\nsize: \{show (size xs)}\nhead: \{fromMaybe "null" (map show (getHead xs))}" ++
    "\ndata: [\{dat}]\n============================" where
      dat = joinBy ", " (map show (lListToList xs))

showBool : Bool -> String
showBool True = "true"
showBool False = "false"

main : IO ()
main = do
  let ls : LList Nat 9 = listToLList [1, 2, 3, 4, 5, 6, 7, 8, 9]

  printLn ls
  let ls = addAt ls 0 10000
  let ls = addAt ls 3 333
  let ls = addEnd ls 111

  printLn ls
  let (_, ls) = removeAt ls 3
  let (_, ls) = removeAt ls 0
  let (_, ls) = removeAt ls 9

  printLn ls
  let (_, ls) = set ls 5 555
  printLn ls
  putStrLn "get: \{show (get ls 5)}"
  putStrLn "contains 5: \{showBool (contains ls 5)}"
  putStrLn "contains 5: \{showBool (contains ls 6)}"

  let ls : LList Nat _ = clear
  printLn ls