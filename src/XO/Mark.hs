module XO.Mark
  ( Mark(..)
  , swap
  )
  where


data Mark
  = X
  | O
  deriving (Eq, Show)


swap :: Mark -> Mark
swap X = O
swap O = X
