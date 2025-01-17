initialNumber :: Int
initialNumber = 100

toTernary :: Int -> Maybe String
toTernary 0 = Just "0"
toTernary n
  | n < 0 = Nothing
  | otherwise = Just (convert n)
  where
    convert 0 = ""
    convert x = convert quotient ++ show remainder
      where
        remainder = x `mod` 3
        quotient = x `div` 3

countChar :: String -> Char -> Int
countChar string targetChar = length (filter (targetChar ==) string)

main :: IO ()
main = do
  putStrLn ("Decimal representation: " ++ show initialNumber)

  let ternary =
        ( case toTernary initialNumber of
            Just t -> t
            Nothing -> error "no negative numbers please"
        )
  putStrLn ("Ternary representation: " ++ ternary)
  let zeroCount = countChar ternary '0'
      oneCount = countChar ternary '1'
      twoCount = countChar ternary '2'
  putStrLn (show zeroCount ++ " zeros, " ++ show oneCount ++ " ones, and " ++ show twoCount ++ " twos")
  let digitSum = oneCount + twoCount * 2
  putStrLn
    ( case digitSum `mod` 5 of
        0 -> "The ternary digits sum to a multiple of 5!"
        1 -> "The ternary digits almost summed to a multiple of 5!"
        4 -> "So close!"
        _ -> "Nope!"
    )

  let zeroMostUsed = all (zeroCount >) [oneCount, twoCount]
  putStrLn ("Zero is" ++ (if zeroMostUsed then " " else " not ") ++ "the most used digit")
