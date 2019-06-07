-- | @X@s and @O@s.
module XO.Mark (Mark(..), swap) where


-- | A player can mark a space using either an @X@ or an @O@.
data Mark = X | O deriving Eq


-- | >>> show X
-- "x"
--
-- >>> show O
-- "o"
instance Show Mark where
  show X = "x"
  show O = "o"


-- | Switches one mark for the other.
--
-- >>> swap X
-- O
--
-- >>> swap O
-- X
swap :: Mark -> Mark
swap X = O
swap O = X
