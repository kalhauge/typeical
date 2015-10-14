module Text.Typeical.Writers.SyntaxTree (showSyntaxTree) where

import Text.Typeical.Writers.BNF (showTerm);
import Text.Typeical.Gramma;

showSyntaxTree :: SyntaxTree -> ShowS
showSyntaxTree st = showSyntaxTreeIndent 0 st . showString "\n"

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


