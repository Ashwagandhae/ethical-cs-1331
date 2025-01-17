module Main
import Decidable.Equality
import Data.List
import Data.List.Views
import Data.String
import Data.Maybe


record IceCream where
  constructor MkIceCream
  scoops : Nat
  cone : Bool
Eq IceCream where (MkIceCream s1 c1) == (MkIceCream s2 c2) = (s1, c1) == (s2, c2)

record Cake where
  constructor MkCake
  frosting : String
Eq Cake where (MkCake f1) == (MkCake f2) = f1 == f2

data DessertType = I IceCream | C Cake

record Dessert where
  constructor MkDessert
  flavor : String
  sweetness : Double
  type : DessertType
Show Dessert where
  show d = case d.type of
    I iceCream => let has = if iceCream.cone then "has" else "does not have" in
      "\{d.flavor} ice cream with \{show iceCream.scoops} scoops and \{has} a cone."
    C cake => "\{d.flavor} cake with a \{cake.frosting} frosting and has a sweetness of \{show d.sweetness}."
Eq Dessert where (MkDessert f1 s1 _) == (MkDessert f2 s2 _) = (f1, s1) == (f2, s2)
Ord Dessert where
  compare (MkDessert f1 s1 _) (MkDessert f2 s2 _) =
    case compare s1 s2 of
      EQ => compare f1 f2
      c => c

mkIceCream : String -> Double -> Nat -> Bool -> Dessert
mkIceCream flavor sweetness scoops cone = MkDessert flavor sweetness $ I (MkIceCream scoops cone)

mkCake : String -> Double -> String -> Dessert
mkCake flavor sweetness frosting = MkDessert flavor sweetness $ C (MkCake frosting)

data Store : (sorted : Bool) -> Type where
  MkStore : (sorted : Bool) -> String -> List Dessert -> Store sorted

mkStore : String -> Store True
mkStore n = MkStore True n []

(.desserts) : Store sr -> List Dessert
(.desserts) (MkStore _ _ desserts) = desserts

addDessert : Dessert -> Store sr -> Store False
addDessert d (MkStore sr name ds) = MkStore False name (ds ++ [d])

findDessert : Dessert -> Store True -> Maybe Dessert
findDessert target s = findDessertRec s.desserts
  where
    findDessertRec : List Dessert -> Maybe Dessert
    findDessertRec l =
        case divideList l of
          Just (lows, mid, highs) =>
            case compare target mid of
              EQ => Just mid
              GT => findDessertRec highs
              LT => findDessertRec lows
          Nothing => Nothing
        where
          divideList : List Dessert -> Maybe (List Dessert, Dessert, List Dessert)
          divideList l = case split l of
            SplitNil => Nothing
            SplitOne x => Just (Nil, x, Nil)
            SplitPair x xs y ys => Just (x :: xs, y, ys)

sortStore : Store False -> Store True
sortStore (MkStore False name ds) = MkStore True name (sort ds)

countGreaterDesserts : Dessert -> Store sr -> Nat
countGreaterDesserts d (MkStore sr name ds) = length $ filter (>= d) ds

showMenu : Store sr -> String
showMenu (MkStore _ name ds) = joinBy "\n" $ "\{name}'s Menu of the Day:" :: (map show ds)

compareStores : Store sr -> Store True -> Bool
compareStores (MkStore _ _ ds) store2 = all (\d => isJust $ findDessert d store2) ds

shop : Dessert -> Store True -> Bool
shop d s = isJust $ findDessert d s

findAvailableStores : List (Store True) -> Dessert -> List (Store True)
findAvailableStores ss d = filter (isJust . findDessert d) ss

main : IO ()
main = do
  let store1 = mkStore "Joshua's store"

  let dessert1 = mkIceCream "vanilla" 45.0 12 True
  let dessert2 = mkIceCream "mint" 12.0 100 False
  let dessert3 = mkCake "cheese" 10.0 "ice"

  let store1 = addDessert dessert1 store1
  let store1 = addDessert dessert2 store1
  let store1 = addDessert dessert3 store1

  putStrLn (showMenu store1)
  let store1 = sortStore store1

  putStrLn "Post sort:"
  putStrLn (showMenu store1)

  let store2 = mkStore "Jeffery's store"

  let dessert4 = mkCake "chocolate" 45.0 "vanilla"
  let dessert5 = mkIceCream "vanilla" 45.0 1 False

  let store2 = addDessert dessert4 store2
  let store2 = addDessert dessert5 store2
  let store2 = sortStore store2

  putStrLn $ "compareStores " ++ if compareStores store1 store2 then "true" else "false"
  putStrLn $ "shop " ++ if shop dessert1 store1 then "true" else "false"
  putStrLn $ "shop " ++ if shop dessert4 store1 then "true" else "false"

  let stores = [store1, store2]

  putStrLn "availabeStores:"
  _ <- sequence $ map (putStrLn . showMenu) (findAvailableStores stores dessert2)

  pure ()