module Text.Typeical.Writers.SyntaxTree ( showSyntaxTree
                                        , writeSyntaxTree
                                        , showSyntaxExpr
                                        , writeSyntaxExpr) where

import Text.Typeical.Writers.BNF (showTerm);
import Text.Typeical.Gramma;

import Data.List;

writeSyntaxTree :: SyntaxTree -> String
writeSyntaxTree = flip showSyntaxTree ""

showSyntaxTree :: SyntaxTree -> ShowS
showSyntaxTree st = showSyntaxTreeIndent 0 st . showString "\n"

writeSyntaxExpr :: SyntaxTree -> String
writeSyntaxExpr = flip showSyntaxExpr ""

showSyntaxExpr :: SyntaxTree -> ShowS
showSyntaxExpr (SyntaxTree (term, subtrees)) = 
    doAll . reverse . intersperse (showChar ' ') $ printers
    where (_, printers) = foldl helper (subtrees, []) term  
          helper :: ([SyntaxTree], [ShowS]) 
                -> Token 
                -> ([SyntaxTree], [ShowS])
          helper (ss,   ts) (Const c) = (ss, showString c : ts)
          helper (s:ss, ts) (Ref _)   = (ss, showSyntaxExpr s : ts)

showSyntaxTreeIndent :: Int -> SyntaxTree -> ShowS
showSyntaxTreeIndent indent (SyntaxTree (term, subtrees)) = 
    indt . showTerm term . showAll subtrees
    where 
      indt :: ShowS
      indt = showString $ take (indent * 2) $ cycle " "
      showSubTree :: SyntaxTree -> ShowS
      showSubTree tree = showString "\n" . 
        showSyntaxTreeIndent (indent + 1) tree
      showAll :: [SyntaxTree] -> ShowS
      showAll sts = doAll $ map showSubTree sts

doAll :: [ShowS] -> ShowS
doAll = foldr (.) id 


