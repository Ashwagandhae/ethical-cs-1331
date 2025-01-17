import Control.Monad (guard)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Text.Printf (printf)
import Text.Read (readMaybe)

nextInt = do
  input <- getLine
  return (readMaybe input :: Maybe Int)

unwrapNextInt = fmap (fromMaybe (error "bad number")) nextInt

powersOfNumber = do
  putStr "What number would you like to calculate powers of? "
  hFlush stdout
  input <- unwrapNextInt
  putStrLn
    ( case input of
        -1 ->
          "-1 raised to 0 is 1\n\
          \-1 raised to odd powers greater than 0 is -1\n\
          \-1 raised to even powers greater than 0 is 1"
        0 ->
          "0 raised to the 0 is 1\n\
          \0 raised to powers greater than 0 is 0"
        1 -> "1 raised to ANY power is still 1"
        num -> getPowers num
    )
  where
    getPowers num =
      intercalate
        "\n"
        $ map
          (\p -> printf "%d raised to the %d is %d." (num :: Int) (p :: Int) (num ^ p))
          (takeWhile (\p -> abs (num ^ p) < 100) [0 ..])

randomNumber = do
  putStr "What is the max value you want your random number to be? "
  hFlush stdout
  input <- unwrapNextInt
  if input <= 0
    then putStrLn "User input must be positive and non-zero."
    else do
      randomNumber <- randomRIO (1, input)
      putStrLn (printf "Your random number is %d" (randomNumber :: Int))

areaOfShape :: String -> String -> (Double -> Double) -> Int -> IO ()
areaOfShape shapeName attributeName attributeToArea roundDigits = do
  attribute <- randomRIO (0 :: Int, 100)
  let area = attributeToArea (fromIntegral attribute :: Double)
  putStrLn $ printf ("A %s of %s %d has an area of %." ++ show roundDigits ++ "f.") shapeName attributeName attribute area

takeInputAndHandle = do
  input <- nextInt
  case input of
    Just 1 -> powersOfNumber
    Just 2 -> randomNumber
    Just 3 -> areaOfShape "circle" "radius" (\r -> pi * r ^ 2) 2
    Just 4 -> areaOfShape "square" "side length" (^ 2) 0
    _ -> do
      putStrLn "Invalid user input, type a number 1-4."
      takeInputAndHandle

main = do
  putStrLn
    "1, Powers of a Number\n\
    \2, Random Positive Integer with Maximum\n\
    \3, Area of Random Circle\n\
    \4, Area of Random Square\n\
    \What would you like to do?"
  takeInputAndHandle