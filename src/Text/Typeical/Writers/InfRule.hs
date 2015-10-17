module Text.Typeical.Writers.InfRule where

import Text.Typeical.Proof
import Text.Typeical.Writers.SyntaxTree;

import Data.List

import Control.Monad;

padc :: Int -> a -> [a] -> [a]
padc n c s = padr n c $ padl (length s + pad) c s
    where pad = (n - length s) `div` 2

padl :: Int -> a -> [a] -> [a]
padl n a as = replicate (n - length as) a ++ as

padr :: Int -> a -> [a] -> [a]
padr n a as = as ++ replicate (n - length as) a

padcList :: Int -> [String] -> String
padcList n [] = spaces n
padcList n st@(s:rest) = padc size ' ' s ++ padcList (n - size) rest
  where totalPad = n - sum (map length st)
        padding = totalPad `div` length st
        size = length s + padding

-- | Block is a list of strings, which is supposed to be stacked, and
-- be of the same size
type Block = [String]

padcBlocks :: Int -> [[String]] -> [String]
padcBlocks n blocks = map (padcList n) $ transpose eqSizeBlocks
    where depth = maximum $ map length blocks
          eqSizeBlocks :: [Block]
          eqSizeBlocks = map spacesLikeFirst blocks
          spacesLikeFirst :: Block -> Block 
          spacesLikeFirst b = padr depth (spaces $ length $ head b) b

spaces :: Int -> String
spaces n = take n $ cycle " " 

writeDerivation :: Derivation -> String
writeDerivation = intercalate "\n" . listDerivation

listDerivation :: Derivation -> [String]
listDerivation d = padcBlocks size prs ++ [
                   padl size '-' (" (" ++ id' ++ ")")
                 , padc size ' ' cns
                 ]
  where prs = map listDerivation $ subderv d
        cns = writeSyntaxExpr $ fact d
        id' = ruleId (rule d)
        slu = sum $ map (length . head) prs 
        size = maximum [length prs * 4 + slu, length cns + 2, length id' + 6]

writeInfRule :: InfRule -> String
writeInfRule = intercalate "\n" . listInfRule 

listInfRule :: InfRule -> [String]
listInfRule ir = [ padcList size prs
                 , padl size '-' (" (" ++ id' ++ ")")
                 , padc size ' ' cns
                 ]
  where prs = map writeSyntaxExpr $ premises ir
        cns = writeSyntaxExpr $ conclusion ir
        id' = ruleId ir
        slu = sum $ map length prs 
        size = maximum [length prs * 4 + slu, length cns + 2, length id' + 6]
