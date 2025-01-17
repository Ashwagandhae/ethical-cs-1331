module Main
import Data.List.Views
import Data.String

data Color = Red | Green | Blue
price : Color -> Double
price Red = 2.5
price Green = 3.0
price Blue = 3.5
Eq Color where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) _ _ = False
Show Color where
  show Red = "RED"
  show Green = "GREEN"
  show Blue = "BLUE"

record SushiRoll where
  constructor MkSushiRoll
  name : String
  color : Color
Eq SushiRoll where (==) (MkSushiRoll n1 _) (MkSushiRoll n2 _) = n1 == n2
Ord SushiRoll where compare (MkSushiRoll n1 _) (MkSushiRoll n2 _) = compare n1 n2
Show SushiRoll where show (MkSushiRoll n c) = "\{n} \{show c}"

mergeRolls : List SushiRoll -> List SushiRoll -> List SushiRoll
mergeRolls [] ys = ys
mergeRolls xs [] = xs
mergeRolls (x::xs) (y::ys) = if x <= y then x::(mergeRolls xs (y::ys)) else y::(mergeRolls ys (x :: xs))

mergeSortRolls : List SushiRoll -> List SushiRoll
mergeSortRolls xs = case split xs of
  SplitNil => []
  SplitOne x => [x]
  SplitPair l ls r rs => mergeRolls (mergeSortRolls (l::ls)) (mergeSortRolls (r::rs))

mergeOrders : List (List SushiRoll) -> List SushiRoll
mergeOrders lists = foldl mergeRolls [] lists

platesOfColor : List SushiRoll -> Color -> List SushiRoll
platesOfColor xs color = filter (\x => color == x.color) xs

totalPrice : List SushiRoll -> Double
totalPrice xs = sum $ map (price . color) xs

flip : List SushiRoll -> List SushiRoll
flip xs = flip' xs []
  where
    flip' : List SushiRoll -> List SushiRoll -> List SushiRoll
    flip' [] acc = acc
    flip' (x::xs) acc = flip' xs (x::acc)

printList : List SushiRoll -> IO ()
printList xs = putStrLn $ joinBy ", " $ map show xs

main : IO ()
main = do
  let arr = [MkSushiRoll "b" Blue, MkSushiRoll "d" Blue, MkSushiRoll "e" Blue, MkSushiRoll "c" Red, MkSushiRoll "a" Red]
  let arr2 = [MkSushiRoll "1" Blue, MkSushiRoll "x" Blue, MkSushiRoll "y" Blue, MkSushiRoll "z" Blue]
  let arr = mergeSortRolls arr
  printList arr

  let arr3 = mergeOrders [arr, arr2]
  printList arr3

  let arr4 = platesOfColor arr3 Red
  printList arr4

  printLn $ totalPrice arr3

  let arr3 = flip arr3
  printList arr3