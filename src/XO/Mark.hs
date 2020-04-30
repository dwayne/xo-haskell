-- | @X@s and @O@s.
--
-- The symbols that can be marked on tiles.
module XO.Mark (Mark(X, O), swap) where


-- | A player can mark a tile using either an @X@ or an @O@.
data Mark = X | O deriving (Eq, Show)


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
