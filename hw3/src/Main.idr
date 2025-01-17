import Data.Fin
import Builtin
import Decidable.Equality
import Data.Nat
import Data.Vect
import Data.Maybe

data Above : Nat -> Type where
  MkAbove : (n : Nat) -> (LT bound n) -> Above bound
Eq (Above a) where MkAbove n _ == MkAbove m _ = n == m
Show (Above b) where show (MkAbove n _) = show n

natToAbove : (n : Nat) -> (bound : Nat) -> Maybe (Above bound)
natToAbove n bound with (isLT bound n)
  natToAbove n bound | (Yes prf) = Just $ MkAbove n prf
  natToAbove n bound | (No _)    = Nothing

integerToAbove : Integer -> (bound : Nat) -> Maybe (Above bound)
integerToAbove x bound = if x >= 0 then natToAbove (cast x) bound else Nothing

fromInteger : (x : Integer) -> {bound : Nat} ->
              {auto prf : (IsJust (integerToAbove x bound))} ->
              Above bound
fromInteger {bound} x {prf} with (integerToAbove x bound)
  fromInteger {bound} x {prf = ItIsJust} | Just y = y

cast : (Above b) -> Nat
cast (MkAbove n _) = n

record Task where
    constructor MkTask
    name : String
    cpuCost : Above 7
Eq Task where MkTask a1 b1  == MkTask a2 b2 = (a1, b1) == (a2, b2)
Show Task where show t = "\{t.name} has CPU cost of \{show t.cpuCost}"

subtractNat : Nat -> Nat -> Maybe Nat
subtractNat n Z = Just n
subtractNat Z m = Nothing
subtractNat (S n) (S m) = subtractNat n m

record Device (size : Nat) where
  constructor MkDevice
  serialNumber : Int
  cpuCapacity : Nat
  cpuRemaining : Nat
  tasks : Vect size (Maybe Task)
Eq (Device n) where
  MkDevice a1 b1 c1 d1 == MkDevice a2 b2 c2 d2 = (a1, b1, c1, d1) == (a2, b2, c2, d2)
Show (Device n) where
  show d = "Device with serial number \{show d.serialNumber} has \{show d.cpuRemaining}"
        ++ " of \{show d.cpuCapacity} CPU remaining."

processDeviceTask : Task -> Device n -> Maybe (Device n)
processDeviceTask task device = do
  index <- findIndex (== Just task) device.tasks
  pure $ {tasks $= replaceAt index (Nothing), cpuRemaining $= (+ cast task.cpuCost) } device

record CellPhone (size : Nat) where
  constructor MkCellPhone
  tasksCompleted : Int
  device : Device size
Eq (CellPhone n) where MkCellPhone a1 b1 == MkCellPhone a2 b2 = (a1, b1) == (a2, b2)
Show (CellPhone n) where show p = show p.device ++ " It has completed \{show p.tasksCompleted} tasks."



ru : (b -> a -> Maybe a) -> (b -> a -> (a, Bool))
ru f b a = case f b a of
    Just newA => (newA, True)
    Nothing => (a, False)

interface IsDevice a where
  addTask : Task -> a -> Maybe a
  processTask : Task -> a -> Maybe a
  addTaskRu : Task -> a -> (a, Bool)
  addTaskRu = ru addTask
  processTaskRu : Task -> a -> (a, Bool)
  processTaskRu = ru processTask
  processTaskPrint : Task -> a -> IO (a, Bool)
  processTaskPrint t a = do
    let (a, res) = processTaskRu t a
    if res then putStrLn "Processed: \{show t}" else pure ()
    pure (a, res)

IsDevice (CellPhone n) where
  addTask task phone = do
    newCpuRemaining <- subtractNat (phone.device.cpuRemaining) (cast task.cpuCost)
    index <- findIndex (not . isJust) phone.device.tasks
    pure $
      let newTasks : Vect n (Maybe Task) = replaceAt index (Just task) phone.device.tasks
          newDevice : Device n = {tasks := newTasks, cpuRemaining := newCpuRemaining} (phone.device)
      in {device := newDevice} phone
  processTask task phone = do
    newDevice <- processDeviceTask task phone.device
    pure $ {device := newDevice, tasksCompleted $= (+1)} phone

record Laptop (size : Nat) where
  constructor MkLaptop
  overclockable : Bool
  device : Device size
Eq (Laptop n) where MkLaptop a1 b1 == MkLaptop a2 b2 = (a1, b1) == (a2, b2)
Show (Laptop n) where
  show l = show l.device ++ " This laptop does\{if l.overclockable then "" else " not"} have overclocking."

bufferSlotsRequired : Laptop n -> Nat -> Nat
bufferSlotsRequired laptop cpuRemaining =
  if (length laptop.device.tasks) <= 4 then 0 else if cpuRemaining < 128 then 2 else 1

IsDevice (Laptop n) where
  addTask task laptop = do
    (newCpuRemaining, newOverclockable) <-
        let normal : Maybe (Nat, Bool) = do
              remainingCpu <- subtractNat (laptop.device.cpuRemaining) (cast task.cpuCost)
              Just (remainingCpu, laptop.overclockable)
            overclocked = do
              guard laptop.overclockable
              remainingCpu <-
                subtractNat (laptop.device.cpuRemaining + div laptop.device.cpuCapacity 4) (cast task.cpuCost)
              Just (remainingCpu, False)
        in normal <|> overclocked
    index <- do
      _ <- let (emptySlots ** _) = filter (not . isJust) laptop.device.tasks
           in emptySlots `subtractNat` (bufferSlotsRequired laptop newCpuRemaining + 1)
      findIndex (not . isJust) laptop.device.tasks
    pure $
      let newTasks : Vect n (Maybe Task) = replaceAt index (Just task) laptop.device.tasks
          newDevice : Device n = {tasks := newTasks, cpuRemaining := newCpuRemaining} (laptop.device)
      in {device := newDevice, overclockable := newOverclockable} laptop
  processTask task laptop = do
    newDevice <- processDeviceTask task laptop.device
    pure $ {device := newDevice} laptop




main : IO ()
main = do
    let addTask = addTaskRu
        processTask = processTaskRu
    let task1 = MkTask "Leghorn" 12
        task2 = MkTask "Silkie" 10
        task3 = MkTask "Rhode Island Red" 16
        task4 = MkTask "Brahma" 8
        task5 = MkTask "Plymouth Rock" 20
        task6 = MkTask "Americana" 41
    let phone1 = MkCellPhone 0 (MkDevice 101 40 40 (Vect.replicate 10 Nothing))
        phone2 = MkCellPhone 0 (MkDevice 102 50 50 (Vect.replicate 10 Nothing))
    putStrLn "adding tasks to phone1"
    let (phone1, couldAdd) = addTask task1 phone1
    putStrLn $ "could " ++ (if couldAdd then "" else "not ") ++ "add task1"
    let (phone1, _) = addTask task2 phone1
    let (phone1, _) = addTask task3 phone1

    putStrLn "processing task from phone1"
    (phone1, _) <- processTaskPrint task1 phone1

    putStrLn "phone1:"
    putStrLn (show phone1)

    let arePhonesEqual = phone1 == phone2
    putStrLn $ "phone1 and phone2 equal? " ++ (if arePhonesEqual then "true" else "false")
    putStrLn $ "phone1 and phone1 equal? " ++ (if phone1 == phone1 then "true" else "false")

    let laptop1 = MkLaptop True (MkDevice 201 60 60 (Vect.replicate 5 Nothing))
        laptop2 = MkLaptop False (MkDevice 202 80 80 (Vect.replicate 6 Nothing))

    putStrLn "adding tasks to laptop1"
    let (laptop1, _) = addTask task4 laptop1
    let (laptop1, _) = addTask task5 laptop1

    putStrLn "processing task from laptop1"
    (laptop1, _) <- processTaskPrint task4 laptop1

    putStrLn "laptop1 details:"
    putStrLn (show laptop1)

    putStrLn "adding tasks to laptop1 until overclock"
    let (laptop1, _) = addTask task6 laptop1

    putStrLn "laptop1 details:"
    putStrLn (show laptop1)

    putStrLn $ "laptop1 and laptop2 equal? false" -- types already enforce this
