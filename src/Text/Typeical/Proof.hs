-- This module contains the elements needed for performing
-- proofs.

module Text.Typeical.Proof where

import Text.Typeical.Gramma
import Data.Map as M

newtype Judgement = Judgement { syntax :: Term 
                              } deriving (Show)

data InfRule = InfRule { ruleId :: String 
                       , premisses :: [SyntaxTree]
                       , conclusion :: SyntaxTree 
                       } deriving (Show)

match = undefined
