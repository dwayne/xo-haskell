module XO.CLI.Player
  ( Player(..)
  , numHumans
  )
  where


data Player
  = Human
  | Computer


instance Show Player where
  show Human = "human"
  show Computer = "computer"


numHumans :: Player -> Player -> Int
numHumans Human Human = 2
numHumans Human Computer = 1
numHumans Computer Human = 1
numHumans Computer Computer = 0
