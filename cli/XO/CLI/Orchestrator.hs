module XO.CLI.Orchestrator
  ( Config, Mark(..), Player(..)
  , mkConfig
  , run
  )
  where


import XO.Mark

import qualified XO.CLI.Orchestrator.Interactive as Interactive
import qualified XO.CLI.Orchestrator.Noninteractive as Noninteractive
import XO.CLI.Player


data Config = Config
  { x :: Player
  , o :: Player
  , first :: Mark
  , rounds :: Int
  }


mkConfig :: Player -> Player -> Mark -> Int -> Maybe Config
mkConfig x o first rounds =
  if rounds <= 0 then
    Nothing
  else
    Just (Config x o first rounds)


run :: Config -> IO ()
run (Config Computer Computer first rounds) =
  Noninteractive.run first rounds

run (Config x o first _) =
  Interactive.run x o first
