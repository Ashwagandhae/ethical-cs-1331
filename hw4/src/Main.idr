import Data.Fin
import Builtin
import Decidable.Equality
import Data.Nat
import Data.Vect
import Data.Maybe
import Data.List

data NonBlankChar : Type where
  MkNonBlankChar : (c : Char) -> (isSpace c = False) -> NonBlankChar

Cast NonBlankChar Char where
  cast (MkNonBlankChar c _) = c

charToNonBlankChar : Char -> Maybe NonBlankChar
charToNonBlankChar c with (decEq (isSpace c) False)
  charToNonBlankChar c | Yes prf = Just $ MkNonBlankChar c prf
  charToNonBlankChar _ | _ = Nothing

data ValidList : Type where
  MkValidList : List Char -> NonBlankChar -> List Char -> ValidList

listToValidList : List Char -> Maybe ValidList
listToValidList xs = rec [] xs
  where
    rec : (List Char) -> (List Char) -> Maybe ValidList
    rec prehead [] = Nothing
    rec prehead (x :: xs) = case charToNonBlankChar x of
      Just nonBlank => Just $ MkValidList prehead nonBlank xs
      Nothing => rec (prehead ++ [x]) xs

data ValidString: Type where
  MkValidString: ValidList -> ValidString

stringToValidString : String -> Maybe ValidString
stringToValidString s = map (\xs => MkValidString xs) (listToValidList (unpack s))

fromString : (s : String) -> {auto prf : (IsJust (stringToValidString s))} -> ValidString
fromString s {prf} with (stringToValidString s)
  fromString s {prf = ItIsJust} | Just v = v

Cast ValidString String where
  cast (MkValidString (MkValidList before nonBlank after)) = pack (before ++ ((cast nonBlank) :: after))
Show ValidString where show = cast
Eq ValidString where (==) a b = (show a) == (show b)

record Witch where
  constructor MkWitch
  signatureCackle : ValidString

record Ghost where
  constructor MkGhost
  robberiesConducted : Nat

data Costume = W Witch | G Ghost
Eq Costume where
  G g1 == G g2 = g1.robberiesConducted == g2.robberiesConducted
  W w1 == W w2 = ((length . show) w1.signatureCackle) == ((length . show) w2.signatureCackle)
  _ == _ = True
Ord Costume where
  compare (G g1) (G g2) = compare g1.robberiesConducted g2.robberiesConducted
  compare (W w1) (W w2) = compare ((length . show) w1.signatureCackle) ((length . show) w2.signatureCackle)
  compare _ _ = EQ

record TrickOrTreater where
  constructor MkTrickOrTreater
  name : ValidString
  age : Fin 13
  numCandy : Nat
  costume : Costume

Show TrickOrTreater where
  show t =
    let costumeShow = case t.costume of
          W w => [cast w.signatureCackle]
          _ => []
        list = (map (unpack) ([cast t.name, show t.age, show t.numCandy] ++ costumeShow)) in
    pack $ intercalate (unpack "/") list

Eq TrickOrTreater where
  MkTrickOrTreater a1 a2 a3 a4 == MkTrickOrTreater b1 b2 b3 b4 = (a1, a2, a3, a4) == (b1, b2, b3, b4)
Ord TrickOrTreater where
  compare (MkTrickOrTreater _ age1 numCandy1 costume1) (MkTrickOrTreater _ age2 numCandy2 costume2) =
    compare (numCandy1, age1, costume1) (numCandy2, age2, costume2)


gainCandy : Nat -> TrickOrTreater -> TrickOrTreater
gainCandy n t = { numCandy $= (+n) } t

loseCandy : Nat -> TrickOrTreater -> (Nat, TrickOrTreater)
loseCandy n t =
  let sub : Nat -> Nat -> Nat -> (Nat, Nat)
      sub Z n count = (Z, count)
      sub m Z count = (n, count)
      sub (S m) (S n) count = sub m n (S count) in
  let (res, rem) = sub t.numCandy n 0 in
  (rem, { numCandy := res } t)

trickOrTreat : TrickOrTreater -> IO (TrickOrTreater)
trickOrTreat t = (do
  let (msg, delta) = pair
  putStrLn msg
  pure (gainCandy delta t))
  where
    pair : (String, Nat)
    pair = case (t.costume) of
      W w => ((cast w.signatureCackle) ++ "! I'll get you my pretty!", 3)
      G g => ("Boo! Trick or treat!", 2)

beRobbed : TrickOrTreater -> Maybe (Nat, TrickOrTreater)
beRobbed t = case t.costume of
  W w => Just $ loseCandy 6 t
  _ => Nothing

rob : TrickOrTreater -> TrickOrTreater -> Maybe (TrickOrTreater, TrickOrTreater)
rob actor victim = case actor.costume of
  G g => do
    (candyGain, newVictim) <- beRobbed victim
    let newActor = gainCandy candyGain actor
    pure (newActor, newVictim)
  _ => Nothing

record HalloweenNight n where
  constructor MkHalloweenNight
  cryptKickerFive : Vect n TrickOrTreater
  ghoulGang : Vect n TrickOrTreater

Show (HalloweenNight n) where
  show h = "cryptKickerFive: \{showVec h.cryptKickerFive} versus ghoulGang: \{showVec h.ghoulGang}" where
    showVec : Vect n TrickOrTreater -> String
    showVec vec = pack $ intercalate (unpack ", ") (map (unpack . show) (toList vec))

compareTeams : HalloweenNight n -> String
compareTeams h = showNum $ sum $ map cmpCount (zip h.cryptKickerFive h.ghoulGang) where
  cmpCount : (TrickOrTreater, TrickOrTreater) -> Int
  cmpCount (t1, t2) = case compare t1 t2 of
    GT => 1
    LT => -1
    _ => 0
  showNum : Int -> String
  showNum n = favoredTeam ++ " is favored." where
    favoredTeam : String
    favoredTeam = if n > 0 then "cryptKickerFive" else
                  if n < 0 then "ghoulGang" else "Neither team"

meetsThreshold : Vect n TrickOrTreater -> Nat -> Bool
meetsThreshold vec threshold = candySum >= threshold
  where candySum = sum $ map numCandy vec

takeTurn : Vect n TrickOrTreater -> Vect n TrickOrTreater -> IO (Vect n TrickOrTreater, Vect n TrickOrTreater)
takeTurn actors victims =
  do
    actors <- sequence $ map trickOrTreat actors
    pure $ unzip $ map rob2 (zip actors victims)
    where
      rob2 : (TrickOrTreater, TrickOrTreater) -> (TrickOrTreater, TrickOrTreater)
      rob2 (a, b) = fromMaybe (a, b) (rob a b)

takeTurns : HalloweenNight n -> IO (HalloweenNight n)
takeTurns h = do
  (cryptKickerFive, ghoulGang) <- takeTurn h.cryptKickerFive h.ghoulGang
  (ghoulGang, cryptKickerFive) <- takeTurn ghoulGang cryptKickerFive
  pure $ { cryptKickerFive := cryptKickerFive, ghoulGang := ghoulGang } h

battle : HalloweenNight n -> Nat -> IO (HalloweenNight n)
battle h threshold = do
    newH <- takeTurns h
    battleRec newH threshold where
  battleRec h threshold =
    case (meetsThreshold h.cryptKickerFive threshold, meetsThreshold h.ghoulGang threshold) of
      (True, True) => finish "It is a tie!"
      (True, False) => finish "cryptKickerFive won!"
      (False, True) => finish "ghoulGang won!"
      (False, False) => do
        newH <- takeTurns h
        battleRec newH threshold
    where
      finish : String -> IO (HalloweenNight n)
      finish s = do
        putStrLn s
        pure h

mkWitch : ValidString -> Fin 13 -> Nat -> ValidString -> TrickOrTreater
mkWitch name age numCandy cackle = MkTrickOrTreater name age numCandy (W (MkWitch cackle))

mkGhost : ValidString -> Fin 13 -> Nat -> TrickOrTreater
mkGhost name age numCandy = MkTrickOrTreater name age numCandy (G (MkGhost 0))

main : IO ()
main = do
  let team1 = [
    mkGhost "1 ghost" 8 0,
    mkGhost "1 azurasians the ghost" 11 10,
    mkWitch "1 cheese the witch" 11 10 "hazurasians",
    mkWitch "1 custard the witch" 11 10 "hazurasians",
    mkWitch "1 jeffery the witch" 1 2 "haha"
  ]
  let team2 = [
    mkGhost "2 ghost" 10 0,
    mkWitch "2 azurasians the witch" 11 10 "hazurasians",
    mkGhost "2 cheese ghost" 2 0,
    mkGhost "2 custard ghost" 3 0,
    mkGhost "1 jeffery the ghost" 1 2
  ]
  let night = MkHalloweenNight team1 team2

  putStrLn $ show night
  putStrLn $ compareTeams night
  night <- battle night 70
  pure ()
