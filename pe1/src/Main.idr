module Main

main : IO ()
main = do
  let backRoadMiles: Int = 25
  let highwayMiles: Int = 60
  let hillyMiles: Int = 10
  let currentGasGallons: Int = 20
  let numberOfPeople: Int = 5

  let gallonsPerPerson: Double = 0.0;

  let gallonsPerPerson = gallonsPerPerson + cast (backRoadMiles * 2);
  let gallonsPerPerson = gallonsPerPerson + cast (highwayMiles * 1);
  let gallonsPerPerson = gallonsPerPerson + cast (hillyMiles * 5);
  let gallonsPerPerson = gallonsPerPerson - cast currentGasGallons;
  let gallonsPerPerson = gallonsPerPerson / cast numberOfPeople;

  putStrLn "Each passenger is responsible for \{show gallonsPerPerson} gallons of gas"
