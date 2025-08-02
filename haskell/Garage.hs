module Garage where

-- Code ------------------------------------------------------------

type Fuel    = Int
data Tank    = Tank { capacity :: Fuel, amount :: Fuel }
data Vehicle = Bicycle | Car Tank | Truck Tank deriving Show
data Garage  = Garage Fuel [Vehicle]           deriving Show

refuelAll :: Garage -> Garage
refuelAll (Garage fuel vehicles) =
    Garage fuel' vehicles'
  where
    (fuel',vehicles') = mapAccumL refuel fuel vehicles

refuel :: Fuel -> Vehicle -> (Fuel, Vehicle)
refuel fuel vehicle | fuel <= 0 = (fuel, vehicle)
refuel fuel vehicle = case vehicle of
    Bicycle    -> green
    Car tank   -> fillup tank <&> Car
    Truck tank -> fillup tank <&> Truck
  where
    green = (fuel, vehicle)
    fillup Tank{..} =
        (fuel - delta, Tank capacity $ amount + delta)
      where
        delta = min fuel (capacity - amount)

-- Example ------------------------------------------------------------

instance Show Tank where
    show Tank{..} = show amount ++ "/" ++ show capacity

bicycle = Bicycle
car cap amt   = Car (Tank cap amt)
truck cap amt = Truck (Tank cap amt)

exampleGarage :: Garage
exampleGarage = Garage 100 [
    bicycle,
    car 50 20,
    truck 200 150,
    bicycle,
    car 60 10
    ]

testGarage :: IO ()
testGarage = do
  putStrLn "Original garage:"
  print exampleGarage
  putStrLn "\nAfter refueling all:"
  print (refuelAll exampleGarage)
