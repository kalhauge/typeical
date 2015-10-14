-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Text.Typeical.Control ( evalT
                             , runT
                             , lift
                             , extendGramma
                             , Typeical
                             , TypeicalT
                             , getGramma
                             , activeSymbols
                             , addJudgement
                             , getJudgements
                             ) where

import Control.Monad.State;
import Control.Monad.Trans;
import Control.Monad.Identity;

import Text.Typeical.Gramma as Gramma;
import Text.Typeical.Proof as Proof;

data TypeicalState = TypeicalState { gramma :: Gramma
                                   , judgements :: [Judgement]
                                   } deriving Show

putJudgements :: TypeicalState -> [Judgement] -> TypeicalState
putJudgements ts js = ts { judgements = js }

putGramma :: TypeicalState -> Gramma -> TypeicalState
putGramma ts g = ts { gramma = g }

modifyJudgements :: TypeicalState -> ([Judgement] -> [Judgement]) -> TypeicalState
modifyJudgements ts f = putJudgements ts $ f $ judgements ts 

modifyGramma :: TypeicalState -> (Gramma -> Gramma) -> TypeicalState
modifyGramma ts f = putGramma ts $ f $ gramma ts 

emptyState :: TypeicalState
emptyState = TypeicalState Gramma.empty []

type TypeicalT = StateT TypeicalState
type Typeical = TypeicalT Identity 

evalT :: Monad m => TypeicalT m a -> m a
evalT s = evalStateT s emptyState

runT :: Monad m => TypeicalT m a -> m (a, TypeicalState)
runT s = runStateT s emptyState

getGramma :: Monad m => TypeicalT m Gramma
getGramma = gramma <$> get

getJudgements :: Monad m => TypeicalT m [Judgement]
getJudgements = judgements <$> get

addJudgement :: Monad m => Judgement -> TypeicalT m ()
addJudgement j = modify $ flip modifyJudgements (j:)

extendGramma :: Monad m => Gramma -> TypeicalT m ()
extendGramma g = modify $ flip modifyGramma (flip extend g) 

activeSymbols :: Monad m => TypeicalT m [Symbol]
activeSymbols = symbols <$> getGramma
