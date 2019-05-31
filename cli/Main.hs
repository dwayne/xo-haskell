module Main (main) where


import qualified System.IO as IO

import XO.CLI.Orchestrator as Orchestrator


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  let Just config = Orchestrator.mkConfig Human Computer X 25
  Orchestrator.run config
