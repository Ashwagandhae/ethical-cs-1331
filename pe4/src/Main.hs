import Control.Monad (mapM_, replicateM)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Random (randomRIO)
import Text.Printf (printf)

data Food
  = JUNK_FOOD
  | DIARY
  | GRAIN
  | PROTEIN
  | FRUIT
  | VEGETABLE
  deriving (Eq, Ord, Enum, Show, Bounded)

mealPrep :: Int -> IO [Food]
mealPrep n = replicateM n $ do
  let min = fromEnum (minBound :: Food)
      max = fromEnum (maxBound :: Food)
  randomIndex <- randomRIO (min, max)
  return $ toEnum randomIndex

followRecipe :: String -> [Food]
followRecipe "" = []
followRecipe string = map (fromMaybe (error "bad word") . convertWord) (words string)
  where
    convertWord word = find ((word ==) . show) [minBound .. maxBound]

mealAnalyzer :: [Food] -> IO ()
mealAnalyzer foods = do
  putStrLn "The following types of food are in your meal:"
  let messages = map createMessage [minBound .. maxBound]
      createMessage variant = printf "%s %d" (show variant) (countForVariant variant)
      countForVariant variant = length $ filter (variant ==) foods
  mapM_ putStrLn messages

healthyChoice :: [Food] -> [Food] -> IO ()
healthyChoice meal1 meal2 =
  let score meal = sum $ map fromEnum meal
      score1 = score meal1
      score2 = score meal2
   in putStrLn $
        printf
          ( case compare score1 score2 of
              GT -> "The first meal is the healthier choice with a score of %d."
              LT -> "The second meal is the healthier choice with a score of %d."
              EQ -> "The two meals are equally healthy with scores of %d."
          )
          (max score1 score2)

main = do
  meal1 <- mealPrep 5
  meal2 <- mealPrep 5

  mealAnalyzer meal1
  mealAnalyzer meal2
  healthyChoice meal1 meal2

  putStrLn "meal 1 should be healthier:"
  let meal1 = followRecipe "JUNK_FOOD JUNK_FOOD VEGETABLE"
  let meal2 = followRecipe "JUNK_FOOD JUNK_FOOD"
  healthyChoice meal1 meal2

  putStrLn "meal 2 should be healthier:"
  let meal1 = followRecipe "JUNK_FOOD JUNK_FOOD VEGETABLE"
  let meal2 = followRecipe "DIARY JUNK_FOOD JUNK_FOOD DIARY DIARY DIARY DIARY DIARY"
  healthyChoice meal1 meal2

  putStrLn "meal 1 should be healthier:"
  let meal1 = followRecipe "DIARY DIARY DIARY DIARY DIARY"
  let meal2 = followRecipe "VEGETABLE"
  healthyChoice meal1 meal2
