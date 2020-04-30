module XO.CLI.Orchestrator (run) where


import XO.CLI.Options
import qualified XO.CLI.Orchestrator.Interactive as Interactive
import qualified XO.CLI.Orchestrator.Noninteractive as Noninteractive
import XO.CLI.Player


run :: Options -> IO ()
run (Options Computer Computer first rounds) = Noninteractive.run first rounds
run (Options x        o        first _)      = Interactive.run first x o
