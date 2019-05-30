module Main (main) where

import qualified System.IO as IO

import XO.Mark

import qualified XO.CLI.Orchestrator.Interactive as Interactive
import XO.CLI.Player


main :: IO ()
main = do
  IO.hSetBuffering IO.stdout IO.NoBuffering
  Interactive.run Human Computer X

-- TODO:
--
-- 1. Implement XO.CLI.Orchestrator.Noninteractive.
