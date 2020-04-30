module XO.CLI.Options (Options(Options), parseOptions) where


import Options.Applicative
import XO.Mark

import XO.CLI.Player


data Options = Options Player Player Mark Int


parseOptions :: IO Options
parseOptions = execParser parserInfo
  where
    parserInfo = info (options <**> helper)
      ( fullDesc
      <> header "xo - A Tic-tac-toe game"
      )


-- Parsers


options :: Parser Options
options = Options
  <$> option player
      ( short 'x'
      <> metavar "PLAYER"
      <> value Human
      <> showDefault
      <> help ("The player in control of x, " ++ playerHelp)
      )
  <*> option player
      ( short 'o'
      <> metavar "PLAYER"
      <> value Computer
      <> showDefault
      <> help ("The player in control of o, " ++ playerHelp)
      )
  <*> option mark
      ( long "first"
      <> short 'f'
      <> metavar "MARK"
      <> value X
      <> showDefault
      <> help "Who plays first, MARK=x|o"
      )
  <*> option positive
      ( long "rounds"
      <> short 'r'
      <> metavar "N"
      <> value 25
      <> showDefault
      <> help "The number of rounds to play between two computer players, N>=1"
      )
  where
    playerHelp = "PLAYER=human|computer"


-- Readers


player :: ReadM Player
player = eitherReader parsePlayer
  where
    parsePlayer "human"    = Right Human
    parsePlayer "computer" = Right Computer
    parsePlayer _          = Left "expected human|computer"


mark :: ReadM Mark
mark = eitherReader parseMark
  where
    parseMark "x" = Right X
    parseMark "o" = Right O
    parseMark _   = Left "expected x|o"


positive :: ReadM Int
positive = eitherReader parsePositive
  where
    parsePositive s =
      case (reads s :: [(Int, String)]) of
        [(n, "")] ->
          if n > 0 then
            Right n
          else
            Left "expected a positive integer"

        _ ->
          Left "expected a positive integer"
