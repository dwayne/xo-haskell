module XO.CLI.Player (Player(Human, Computer), numHumans) where


data Player = Human | Computer deriving (Show)


numHumans :: Player -> Player -> Int
numHumans Human    Human    = 2
numHumans Human    Computer = 1
numHumans Computer Human    = 1
numHumans Computer Computer = 0
