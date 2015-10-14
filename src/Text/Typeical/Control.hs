-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Text.Typeical.Control (evalT) where

import Control.Monad.State;
import Control.Monad.Trans;

import Text.Typeical.Gramma as Gramma;

type TypeicalState = Gramma

emptyState :: TypeicalState
emptyState = Gramma.empty

type TypeicalT = StateT TypeicalState

evalT :: Monad m => TypeicalT m a -> m a
evalT s = evalStateT s emptyState

extendGramma :: Gramma -> TypeicalT m ()
extendGramma g = modify (flip extend g) 

getSyntaxParser :: Symbol -> TypeicalT m (ParserT String m SyntaxTree)
getSyntaxParser s = 

