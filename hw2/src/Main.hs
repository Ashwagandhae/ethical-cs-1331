{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Data.Maybe (fromMaybe, isJust)
import Data.String (IsString (..))
import Data.Void (Void)
import GHC.IO.Handle.Text (memcpy)
import System.Random (Random (random), randomIO, randomRIO)
import Text.Printf (PrintfArg (..), printf)

class IsPositive a where
  mkPositive :: a -> Maybe (Positive a)

newtype Positive a = Positive a

instance IsPositive Double where
  mkPositive y = if not (isNaN y) && not (isInfinite y) && y > 0 then Just (Positive y) else Nothing

instance IsPositive Int where mkPositive y = if y > 0 then Just (Positive y) else Nothing

instance IsPositive [Char] where mkPositive s = if not (null s) then Just (Positive s) else Nothing

instance (IsPositive a, Num a) => Num (Positive a) where
  (+) :: (IsPositive a, Num a) => Positive a -> Positive a -> Positive a
  (Positive x) + (Positive y) = Positive (x + y)
  (Positive x) * (Positive y) = Positive (x * y)
  abs (Positive x) = Positive x
  signum (Positive x) = 1
  fromInteger n = fromMaybe (error "Invalid result") (mkPositive (fromInteger n))
  negate (Positive x) = error "Can't negate positive"

instance (IsPositive a, Fractional a) => Fractional (Positive a) where
  (Positive x) / (Positive y) = Positive (x / y)
  recip (Positive x) = Positive (recip x)
  fromRational r = fromMaybe (error "Invalid result") (mkPositive (fromRational r))

instance (IsPositive a, Eq a) => Eq (Positive a) where Positive x == Positive y = x == y

instance (IsPositive a, Show a) => Show (Positive a) where show (Positive x) = show x

instance (IsPositive a, Ord a) => Ord (Positive a) where compare (Positive x) (Positive y) = compare x y

instance (IsPositive a, Real a) => Real (Positive a) where toRational (Positive x) = toRational x

instance (IsPositive a, Enum a) => Enum (Positive a) where
  toEnum x = Positive (toEnum x)
  fromEnum (Positive x) = fromEnum x

instance (IsPositive a, Integral a) => Integral (Positive a) where
  toInteger (Positive x) = toInteger x
  quotRem (Positive a) (Positive b) = let (x, y) = quotRem a b in (Positive x, Positive y)

instance (IsPositive a, PrintfArg a) => PrintfArg (Positive a) where
  formatArg (Positive x) = formatArg x

instance (IsPositive a, IsString a) => IsString (Positive a) where
  fromString str
    | null str = error "IsPositiveString cannot be empty."
    | otherwise = Positive (fromString str)

unwrapPositive (Positive d) = d

data Fish a = Fish {name :: Positive String, fLength :: Positive Double, weight :: Positive Double, custom :: a}

instance Default (Fish ()) where def = Fish {name = "Nemo", fLength = 5.0, weight = 12.0, custom = ()}

class Describe a where
  describe :: Fish a -> Maybe String

instance (Describe a) => Show (Fish a) where
  show f = baseShow ++ customShow
    where
      baseShow =
        printf
          "I'm a talking fish named %s. My length is %s and my weight is %s."
          (name f)
          (formatLength f)
          (formatWeight f)
      customShow = maybe "" prependSpace (describe f)
      prependSpace s = " " ++ s
      formatLength f = formatValue 12.0 (fLength f) (Normal "ft") (Normal "in")
      formatWeight f = formatValue 16.0 (weight f) (SingularPlural "lb" "lbs") (Normal "oz")

data UnitName = Normal String | SingularPlural String String

showUnitName name amount = case name of
  Normal s -> s
  SingularPlural s p -> if amount == 1 then s else p

formatValue :: Double -> Positive Double -> UnitName -> UnitName -> String
formatValue biggerThreshold smallerAmount biggerName smallerName =
  printf "%d %s %.2f %s" bigger (showUnitName biggerName bigger) smaller (showUnitName smallerName smaller)
  where
    (smaller, bigger) = find (unwrapPositive smallerAmount) 0 :: (Double, Int)
    find s b = if s < biggerThreshold then (s, b) else find (s - biggerThreshold) (b + 1)

instance Describe () where describe f = Nothing

newtype CatFish = CatFish {whiskerLength :: Positive Double}

instance Default (Fish CatFish) where
  def = Fish {name = "Bubba", fLength = 52.0, weight = 720.0, custom = CatFish {whiskerLength = 5.0}}

isShaggy f = unwrapPositive (whiskerLength (custom f)) > unwrapPositive (fLength f)

instance Describe CatFish where
  describe f =
    Just $
      printf
        "I'm a catfish whose longest whisker is %.2f, so I %s shaggy."
        (whiskerLength (custom f))
        (if isShaggy f then "am" else "am not" :: String)

data StripedBass = StripedBass {stripeCount :: Positive Int, isSaltwater :: Bool, bestFriend :: Maybe (Fish CatFish)}

instance Default (Fish StripedBass) where
  def =
    Fish
      { name = "Striper",
        fLength = 30.0,
        weight = 320.0,
        custom = StripedBass {stripeCount = 14, isSaltwater = False, bestFriend = Nothing}
      }

migrate f =
  let stripedBass = custom f
      update = if isJust (bestFriend stripedBass) then id else not
   in f {custom = stripedBass {isSaltwater = update (isSaltwater stripedBass)}}

instance Describe StripedBass where
  describe f = Just $ printf "I'm a %swater striped bass with %d stripes. I have %s." water stripes friend
    where
      water = if isSaltwater (custom f) then "salt" else "fresh" :: String
      stripes = stripeCount (custom f)
      friend = maybe "no best friend" (("a best friend named " ++) . unwrapPositive . name) (bestFriend (custom f))

newtype FlyingFish = FlyingFish {flightTime :: Positive Int}

instance Default (Fish FlyingFish) where
  def = Fish {name = "Gilbert", fLength = 12.0, weight = 24.0, custom = FlyingFish {flightTime = 36}}

calculatePower f = weight f * fromIntegral (flightTime (custom f))

fly f = do
  rand <- randomIO :: IO Double
  let seconds = (1 - rand) * (fromIntegral . unwrapPositive) (flightTime (custom f))
  putStrLn $ printf "Woohoo! %s flew for %.2f seconds." (name f) seconds

instance Describe FlyingFish where
  describe f =
    Just $
      printf
        "I'm a flying fish, and my flight time record is %d, so my power is %.2f"
        (flightTime (custom f))
        (calculatePower f)

main :: IO ()
main = do
  let fish1 = Fish "Donald" 15.0 5.0 ()
      fish2 = fish1
      fish3 = def :: Fish ()
  print fish1
  print fish2
  print fish3

  let catfish1 = Fish "James David" 52.0 720.0 (CatFish 123312312.0)
      catfish2 = catfish1
      catfish3 = def :: Fish CatFish
  print catfish1
  print catfish2
  print catfish3
  putStrLn $ "Is Bubba shaggy? " ++ show (isShaggy catfish1)

  let bass1 = Fish "Tim" 30.0 320.0 (StripedBass 14 True (Just catfish1))
      bass2 = bass1
      bass3 = def :: Fish StripedBass
  print bass1
  print bass2
  print bass3
  let bass1Migrated = migrate bass1
  putStrLn $ "After migrating: " ++ show bass1Migrated

  let flyingFish1 = Fish "Kamala" 12.0 24.0 (FlyingFish 36)
      flyingFish2 = flyingFish1
      flyingFish3 = def :: Fish FlyingFish
  print flyingFish1
  print flyingFish2
  print flyingFish3
  fly flyingFish1
  putStrLn $ "Flying fish power: " ++ show (calculatePower flyingFish1)