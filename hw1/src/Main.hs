import Control.Monad (mfilter)
import Data.Maybe (isJust, mapMaybe)
import Data.Vector (Vector, toList, (!), (//))
import qualified Data.Vector as V
import Text.Printf (printf)

data GarageOwner = GarageOwner
  { name :: String,
    age :: Int
  }

newGarageOwner :: String -> Int -> GarageOwner
newGarageOwner name age = GarageOwner {name = name, age = age}

data Car = Car
  { year :: Int,
    make :: String,
    model :: String,
    color :: String,
    conditionCategory :: Int
  }

newCar :: Int -> String -> String -> String -> Int -> IO Car
newCar year make model color conditionCategory = do
  let c = if conditionCategory > 100 || conditionCategory < 40 then 80 else conditionCategory
  let message
        | c >= 90 = "Perfect"
        | c >= 80 = "Excellent"
        | c >= 70 = "Fine"
        | c >= 60 = "Very Good"
        | c >= 50 = "Good"
        | otherwise = "Driver"
  putStrLn message
  return Car {year = year, make = make, model = model, color = color, conditionCategory = c}

describeCar :: Car -> String
describeCar car = printf "%s %d %s %s" (color car) (year car) (make car) (model car)

-- isRestored = isConditionRestored . conditionCategory

data Garage = Garage
  { theOwner :: GarageOwner,
    carCatalogue :: Vector (Maybe Car)
  }

newGarage :: GarageOwner -> Vector (Maybe Car) -> Garage
newGarage theOwner carCatalogue = Garage {theOwner = theOwner, carCatalogue = carCatalogue}

carsOwned :: Garage -> Int
carsOwned garage = V.length $ V.filter isJust (carCatalogue garage)

data UpdateRes = BadIndex | NoOldCar | OldCar Car

updateCar :: Maybe Car -> Garage -> Int -> (Garage, UpdateRes)
updateCar maybeCar garage i =
  if i >= 0 && i < V.length (carCatalogue garage)
    then
      let catalogue = carCatalogue garage
          oldCar = catalogue ! i
          newCatalogue = catalogue // [(i, maybeCar)]
       in (garage {carCatalogue = newCatalogue}, maybe NoOldCar OldCar oldCar)
    else (garage, BadIndex)

addCar :: Car -> Garage -> Int -> IO (Garage, UpdateRes)
addCar car garage i = do
  let (retGarage, oldCar) = updateCar (Just car) garage i
  putStrLn $ case oldCar of
    OldCar o -> printf "There was a %s here before." (describeCar o)
    NoOldCar -> printf "A %s was just parked here." (describeCar car)
    BadIndex -> "Cannot add car to this spot."
  return (retGarage, oldCar)

sellCar :: Garage -> Int -> IO (Garage, UpdateRes)
sellCar garage i = do
  let (retGarage, oldCar) = updateCar Nothing garage i
  putStrLn $ case oldCar of
    OldCar car -> printf "%s just sold a %s." ((name . theOwner) garage) (describeCar car)
    NoOldCar -> "There was no car to sell!"
    BadIndex -> "There was no car to sell!"
  return (retGarage, oldCar)

showCertainCars :: Garage -> Int -> IO ()
showCertainCars garage conditionFilter = mapM_ printCar $ mapMaybe filterCar $ toList (carCatalogue garage)
  where
    printCar car = putStrLn $ printf "A %s with a condition category of %d" (describeCar car) (conditionCategory car)
    filterCar = mfilter passesConditionFilter
    passesConditionFilter car = conditionCategory car > conditionFilter

main :: IO ()
main = do
  let owner = newGarageOwner "Joseph" 123
      vec = V.replicate 10 Nothing
      garage = newGarage owner vec
  car1 <- newCar 1960 "Jaguar" "E-Type" "silver" 89
  car2 <- newCar 2024 "Jeep" "F-Type" "blue" 80
  car3 <- newCar 2024 "Jeep" "G-Type" "red" 40
  (garage, _) <- addCar car1 garage 0
  (garage, _) <- addCar car2 garage 1
  (garage, _) <- addCar car3 garage 4
  (garage, _) <- sellCar garage 0
  putStrLn $ "Number of cars owned: " ++ show (carsOwned garage) ++ "."
  showCertainCars garage 50

  return ()
