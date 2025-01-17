module Main
import Data.String
import Decidable.Equality
import Data.Maybe
import Data.List1
import System.File

data NonBlankChar : Type where
  MkNonBlankChar : (c : Char) -> (isSpace c = False) -> NonBlankChar

Cast NonBlankChar Char where
  cast (MkNonBlankChar c _) = c

charToNonBlankChar : Char -> Maybe NonBlankChar
charToNonBlankChar c with (decEq (isSpace c) False)
  charToNonBlankChar c | Yes prf = Just $ MkNonBlankChar c prf
  charToNonBlankChar _ | _ = Nothing

data NonBlankString : Type where
  MkNonBlankString : List Char -> NonBlankChar -> List Char -> NonBlankString

stringToNonBlank : String -> Maybe NonBlankString
stringToNonBlank s = rec [] (unpack s)
  where
    rec : (List Char) -> (List Char) -> Maybe NonBlankString
    rec prehead [] = Nothing
    rec prehead (x :: xs) = case charToNonBlankChar x of
      Just nonBlank => Just $ MkNonBlankString prehead nonBlank xs
      Nothing => rec (prehead ++ [x]) xs

fromString : (s : String) -> {auto prf : (IsJust (stringToNonBlank s))} -> NonBlankString
fromString s {prf} with (stringToNonBlank s)
  fromString s {prf = ItIsJust} | Just v = v
Cast NonBlankString String where
  cast (MkNonBlankString before nonBlank after) = pack (before ++ ((cast nonBlank) :: after))
Show NonBlankString where show = cast
Eq NonBlankString where (==) a b = (show a) == (show b)

record Football where
  constructor MkFootball
  performer : NonBlankString

record Basketball where
  constructor MkBasketball
  league : NonBlankString

data Sport = F Football | B Basketball

record SportsGame where
  constructor MkSportsGame
  venue : NonBlankString
  startTime : NonBlankString
  startDate : NonBlankString
  score1 : Nat
  score2 : Nat
  seatsLeft : Nat
  sport : Sport

Show Sport where
  show (F (MkFootball p)) = cast p
  show (B (MkBasketball l)) = cast l

Eq Sport where
  (==) (F (MkFootball p1)) (F (MkFootball p2)) = p1 == p2
  (==) (B (MkBasketball l1)) (B (MkBasketball l2)) = l1 == l2
  (==) _ _ = False

sportTag : Sport -> String
sportTag (F _) = "FootballGame"
sportTag (B _) = "BasketballGame"

Show SportsGame where
  show s = joinBy "," [sportTag s.sport, show s.venue, show s.startTime, show s.startDate,
    show s.score1, show s.score2, show s.seatsLeft, show s.sport]

Eq SportsGame where
  (==) (MkSportsGame a1 b1 c1 d1 e1 f1 g1) (MkSportsGame a2 b2 c2 d2 e2 f2 g2) =
    (a1, b1, c1, d1, e1, f1, g1) == (a2, b2, c2, d2, e2, f2, g2)

data RetrieveError = FileError FileError | ParseError

processInfo : String -> Maybe SportsGame
processInfo str = let tokens = forget (split (==',') str) in case tokens of
  ["BasketballGame", venue, startTime, startDate, score1, score2, seatsLeft, league] =>
    pure $ MkSportsGame !(nb venue) !(nb startTime) !(nb startDate)
      !(nat score1) !(nat score2) !(nat seatsLeft) (B (MkBasketball !(nb league)))
  ["FootballGame", venue, startTime, startDate, score1, score2, seatsLeft, performer] =>
    pure $ MkSportsGame !(nb venue) !(nb startTime) !(nb startDate)
      !(nat score1) !(nat score2) !(nat seatsLeft) (F (MkFootball !(nb performer)))
  _ => Nothing
  where
    nb = stringToNonBlank
    nat : String -> Maybe Nat
    nat = parsePositive



retrieveGames : String -> IO (Either RetrieveError (List SportsGame))
retrieveGames path = do
  file <- readFile $ path
  pure $ case file of
    Left err => Left (FileError err)
    Right contents => maybe (Left ParseError) Right $ sequence (map processInfo (lines contents))

mapLeft : (a -> b) -> Either a c -> Either b c
mapLeft f (Left a) = Left $ f a
mapLeft f (Right r) = (Right r)

writeGames : String -> List SportsGame -> IO (Either RetrieveError ())
writeGames path games = map (mapLeft FileError) (writeFile path (joinBy "\n" (map show games)))

modifyGames : String -> (List SportsGame -> List SportsGame) -> IO (Either RetrieveError ())
modifyGames path modify = do
  games <- retrieveGames path
  case games of
    Right games => writeGames path (modify games)
    Left (FileError FileNotFound) => writeGames path (modify [])
    Left err => pure $ Left err

purchaseTickets : String -> List SportsGame -> IO (Either RetrieveError ())
purchaseTickets path newGames = modifyGames path $ \games => (filter (\g => g.seatsLeft > 0) (games ++ newGames))

enumerate : List a -> List (Nat, a)
enumerate [] = []
enumerate (x::xs) = (length xs, x)::(enumerate xs)

findTickets : String -> SportsGame -> IO (Either RetrieveError (List Nat))
findTickets path targetGame = do
  games <- retrieveGames path
  pure $ map (\games => map fst $ filter (\(_, g) => g == targetGame) (enumerate games)) games

attendGame : String -> SportsGame -> IO (Either RetrieveError ())
attendGame path targetGame = modifyGames path $ filter (/=targetGame)

main : IO ()
main = do
  let path : String = "./TestTickets.csv"
  _ <- removeFile path

  let bgame1 = MkSportsGame "venue1" "startTime" "startDate" 1 2 3 (B (MkBasketball "league"))
  let bgame2 = MkSportsGame "venue2" "startTime" "startDate" 1 2 3 (B (MkBasketball "league"))
  let fgame1 = MkSportsGame "venue1" "startTime" "startDate" 1 2 3 (F (MkFootball "performer"))
  let fgame2 = MkSportsGame "venue2" "startTime" "startDate" 1 2 3 (F (MkFootball "performer"))

  let games = [bgame1, bgame2, fgame1, fgame2]

  _ <- purchaseTickets path games

  let bgame3 = MkSportsGame "venue3" "startTime" "startDate" 1 2 3 (B (MkBasketball "league"))
  let games2 = [bgame3]

  _ <- purchaseTickets path games2

  games <- retrieveGames path

  putStr (case games of
    Left (FileError err) => show err
    Left (ParseError) => "Parse failed"
    Right games => unlines (map show games))

  _ <- attendGame path bgame1

  pure ()

