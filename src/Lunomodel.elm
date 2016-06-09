module Lunomodel exposing (..)

type Role = Pilot | Passenger

type alias Planet = { radius: Float, mass: Float }
type alias Cosmonaut = {  firstName: String,
                          lastName: String,
                          mass: Float,
                          role: Role,
                          maxAcceleration: Float
                       }                  
type alias Spaceship  = {
    mass: Float,
    c: Float,
    fuel: Float,
    persons: List Cosmonaut  
  }


type alias Engine = {
    mass: Float,
    time: Float,
    revers: Bool,
    startedTime: Int,
    started: Bool
    }

moon = Planet 1738000 (7.35 * (10 ^ 22))

totalMass ship = ship.mass + ship.fuel + (List.sum <| List.map .mass ship.persons)
takeFuel ship value = if (ship.fuel - value < 0) then {ship | fuel = 0}
                      else {ship | fuel = ship.fuel - value}
tank ship value = {ship | fuel = ship.fuel + value }                          
addCosmonaut ship cosmonaut = { ship | persons = cosmonaut :: ship.persons }                          
unboard ship = { ship | persons = [] }
getPilot ship = List.filter (\p -> case p.role of
                                     Pilot -> True
                                     Passenger -> False) ship.persons

freeFall : Planet -> Float -> Float
freeFall planet height =
  let h = planet.radius + height
      g = 6.6740831 * (10 ^ -11)
  in planet.mass * g / (h ^ 2) 

round2 val = toString (toFloat (floor (100 * val)) / 100)
