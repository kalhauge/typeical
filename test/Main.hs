import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import Text.Typeical

import Text.Parsec

import Text.Typeical.Readers.SyntaxTree
import Text.Typeical.Readers.BNF

import Text.Typeical.Proof
import Text.Typeical.Gramma as Gramma
import Text.Typeical.Parsing

tests = [
      parseTests
    , matchTests
    ]

baselangStr = "t ::= v | ( t )\nv ::= true | false\n"
baselang = fromList [ t, v ]
t = (Symbol "t", [ tV, tParan ])
v = (Symbol "v", [ tTrue , tFalse ])
tV = [Ref Symbol {symbolName = "v"}]
tParan = [Const "(",Ref Symbol {symbolName = "t"},Const ")"]
tTrue = [Const "true"]
tFalse = [Const "false"]

jmt = [Ref (Symbol "t"), Const "-->", Ref (Symbol "t")]

parseTests :: Test.Framework.Test
parseTests = testGroup "parse tests" [
    testCase "base language" testBaseLang
  , testCase "true" testSimpleSyntax
  , testCase "( true )" testNested
  ]

pST :: Parser String SyntaxTree
pST = syntaxTree baselang (Symbol "t")

sT = SyntaxTree

testBaseLang = 
    Right baselang @=? 
    parseStr (bnf Gramma.empty) baselangStr
testSimpleSyntax = 
    Right (SyntaxTree tV [SyntaxTree tTrue []]) @=?
    parseStr pST "true"
testNested = 
    Right (SyntaxTree tParan [SyntaxTree tV [SyntaxTree tTrue []]]) @=? 
    parseStr pST "( true )"


matchTests :: Test.Framework.Test
matchTests = testGroup "match tests" [
    testCase "simple match" testSimpleMatch
  , testCase "simple mismatch" testSimpleMismatch
  , testCase "variable match" testVarMatch
  , testCase "match multible variables" testVarMultiMatch
  , testCase "match nested variables" testNestedMatch
  ]

e0 e = SyntaxTree e []
e1 e s = SyntaxTree e [s]
eM = SyntaxTree

eV = e1 tV 
eJ = eM jmt

varT = Variable (Symbol "t")
vT m n = Var (varT m n)

eParan = e1 tParan
eTrue = eV $ e0 tTrue
eFalse = eV $ e0 tFalse

testSimpleMatch =
    Just (Match (emptySolution, emptySolution)) @=?
    match eTrue eTrue

testSimpleMismatch =
    Nothing @=? match eTrue eFalse

testVarMatch =
    Just (Match (solution [(varT 0 0, eTrue)], emptySolution)) 
    @=?  match (vT 0 0) eTrue

testVarMultiMatch =
    Just (Match (solution [(varT 0 0, eTrue)], solution [(varT 1 0, eTrue)])) 
    @=?  match (eJ [vT 0 0, vT 0 0]) (eJ [eTrue, vT 1 0])

testNestedMatch =
    Just (Match (
         solution [(varT (-1) 0, vT 0 0)], 
         solution [(varT (-1) 0, eParan (vT 0 0))])) 
    @=? match (eParan (vT (-1) 0)) (vT (-1) 0)



main :: IO ()
main = defaultMainWithOpts tests mempty
