-- Haskell: https://gist.github.com/instinctive/6c06b1d257b9c8f567fb7fb74f484540
-- Claude: https://claude.ai/chat/7a0eb410-ecea-44c6-b10e-3544a5bcd1c5

module Garage

%default total

Fuel : Type
Fuel = Nat

record Tank where
  constructor MkTank
  capacity : Fuel
  amount   : Fuel

data HasTank = Yes | No

data Vehicle : HasTank -> Type where
  Bicycle : Vehicle No
  Car     : Tank -> Vehicle Yes
  Truck   : Tank -> Vehicle Yes

record Garage where
  constructor MkGarage
  fuel     : Fuel
  vehicles : List (t ** Vehicle t)

refuel : Fuel -> Vehicle Yes -> (Fuel, Vehicle Yes)
refuel fuel vehicle = 
  case fuel of
    Z => (fuel, vehicle)
    _ => fillup vehicle
  where
    refillTank : Fuel -> Tank -> (Fuel, Tank)
    refillTank fuel (MkTank cap amt) =
      let delta = min fuel (minus cap amt)
      in (minus fuel delta, MkTank cap $ amt + delta)

    fillup : Vehicle Yes -> (Fuel, Vehicle Yes)
    fillup (Car tank) = map Car (refillTank fuel tank)
    fillup (Truck tank) = map Truck (refillTank fuel tank)

refuelAnyVehicle : Fuel -> (t ** Vehicle t) -> (Fuel, (t ** Vehicle t))
refuelAnyVehicle fuel (No ** v) = (fuel, (No ** v))
refuelAnyVehicle fuel (Yes ** v) = 
  let (fuel', v') = refuel fuel v in (fuel', (Yes ** v'))

mapAccumL : (acc -> a -> (acc,b)) -> acc -> List a -> (acc,List b)
mapAccumL f acc [] = (acc,[])
mapAccumL f acc (x::xx) =
    let (acc',y) = f acc x
        (acc'',yy) = mapAccumL f acc' xx
    in (acc'',y::yy)

refuelAll : Garage -> Garage
refuelAll (MkGarage fuel vehicles) =
  let (fuel', vehicles') = mapAccumL refuelAnyVehicle fuel vehicles
  in MkGarage fuel' vehicles'

-- Example

bicycle : (No ** Vehicle No)
bicycle = (No ** Bicycle)

car   : Fuel -> Fuel -> (Yes ** Vehicle Yes)
truck : Fuel -> Fuel -> (Yes ** Vehicle Yes)
car   cap amt = (Yes ** Car   (MkTank cap amt))
truck cap amt = (Yes ** Truck (MkTank cap amt))

Show Tank where
  show (MkTank cap amt) = show amt ++ "/" ++ show cap

Show (Vehicle hasTank) where
  show Bicycle      = "Bicycle"
  show (Car tank)   = "Car " ++ show tank
  show (Truck tank) = "Truck " ++ show tank

Show (t ** Vehicle t) where
  show (_ ** v) = show v

Show Garage where
  show (MkGarage f vs) = "Garage " ++ show f ++ " " ++ show vs

exampleGarage : Garage
exampleGarage = MkGarage 100 [
  bicycle,
  car 50 20,
  truck 200 150,
  bicycle,
  car 60 10
]

testGarage : IO ()
testGarage = do
  putStrLn "Original garage:"
  printLn exampleGarage
  putStrLn "\nAfter refueling all:"
  printLn (refuelAll exampleGarage)
