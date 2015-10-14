-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof where

import Text.Typeical.Gramma

newtype Judgement = Judgement { syntax :: Term 
                              } deriving (Show)

data InfRule = InfRule { id :: String 
                       , premisses :: [SyntaxTree]
                       , conclusion :: SyntaxTree
                       } deriving (Show)

