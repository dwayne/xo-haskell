module Main (main) where


import qualified System.IO as IO

import XO.CLI.Options (parseOptions)
import XO.CLI.Orchestrator (run)


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  parseOptions >>= run
