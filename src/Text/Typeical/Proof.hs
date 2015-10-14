-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof where

import Text.Typeical.Gramma

newtype Judgement = Judgement { syntax :: Term 
                              } deriving (Show)

-- data Var = Var { symbol :: Symbol, version :: (Int, Int) }

data InfrRule = InfrRule { id :: String 
                         --, premisses :: [Fact]
                         --, conclusion :: Fact
                         } deriving (Show)
