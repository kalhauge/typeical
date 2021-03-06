import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import Text.Typeical

import Text.Parsec

import Text.Typeical.Writers.SyntaxTree

import Text.Typeical.Readers.SyntaxTree
import Text.Typeical.Readers.BNF

import Text.Typeical.Proof
import Text.Typeical.Gramma as Gramma
import Text.Typeical.Parsing

tests = [
      parseTests
    , matchTests
    ]

baselangStr = "t ::= v | ( t ) | t & t \nv ::= true | false\n"
baselang = fromList [ t, v ]
t = (Symbol "t", [ tV, tParan, tAnd ])
v = (Symbol "v", [ tTrue , tFalse ])
tV = [Ref Symbol {symbolName = "v"}]
tParan = [Const "(",Ref Symbol {symbolName = "t"},Const ")"]
tAnd = [Ref (Symbol "t"), Const "&", Ref (Symbol "t")]
tTrue = [Const "true"]
tFalse = [Const "false"]

jmt = [Ref (Symbol "t"), Const "-->", Ref (Symbol "t")]

testParse :: SyntaxTree -> Test.Framework.Test
testParse s = 
    testCase str $ Right s @=? parseStr pST str
  where str = writeSyntaxExpr s

parseTests :: Test.Framework.Test
parseTests = testGroup "parse tests" $
    testCase "base language" testBaseLang
  :  map testParse [ 
      eTrue
    , eParan eTrue
    , eTrue `eAndi` eTrue
    , vT 0 0 
    , vT 0 0 `eAndi` vT 1 3
  ]

pST :: Parser String SyntaxTree
pST = syntaxTree baselang (Symbol "t")

sT = SyntaxTree

testBaseLang = 
    Right baselang @=? 
    parseStr (bnf Gramma.empty) baselangStr


matchTests :: Test.Framework.Test
matchTests = testGroup "match tests" [
    testCase "simple match" testSimpleMatch
  , testCase "simple mismatch" testSimpleMismatch
  , testCase "variable match" testVarMatch
  --, testCase "match multible variables" testVarMultiMatch
  -- , testCase "match nested variables" testNestedMatch
  -- , testCase "match multi nested variables" testMulitNestedMatch 
  ]

e0 e = SyntaxTree e []
e1 e s = SyntaxTree e [s]
e2 e s1 s2  = SyntaxTree e [s1, s2]
eM = SyntaxTree

eV = e1 tV 
eJ = eM jmt
eJi = e2 jmt

varT = Variable (Symbol "t")
vT m n = Var (varT m n)

eAndi = e2 tAnd
eParan = e1 tParan
eTrue = eV $ e0 tTrue
eFalse = eV $ e0 tFalse

testSimpleMatch =
    Just emptySolution @=?  match eTrue eTrue

testSimpleMismatch =
    Nothing @=? match eTrue eFalse

testVarMatch =
    Just (solution [(varT 0 0, eTrue)]) 
    @=?  match (vT 0 0) eTrue

testVarMultiMatch =
    Just (solution [(varT 0 0, eTrue)]) 
    @=?  match (eJ [vT 0 0, vT 0 0]) (eJ [eTrue, vT 1 0])

testNestedMatch =
    Just (solution [(varT (-1) 0, vT 0 0)]) 
    @=? match (eParan (vT (-1) 0)) (vT (-1) 0)

testMulitNestedMatch =
    Just (solution [(varT (-1) 0, eParan eTrue), 
                   (varT (-1) 1, vT (-1) 2)]) 
    @=? match (eParan (vT (-1) 0) `eJi` eParan(vT (-1) 1)) 
              (eParan (eParan eTrue) `eJi` eParan(vT (-1) 2))



main :: IO ()
main = defaultMainWithOpts tests mempty
