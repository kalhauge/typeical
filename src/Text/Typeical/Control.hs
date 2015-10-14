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
                             ) where

import Control.Monad.State;
import Control.Monad.Trans;
import Control.Monad.Identity;

import Text.Typeical.Gramma as Gramma;

type TypeicalState = Gramma

emptyState :: TypeicalState
emptyState = Gramma.empty

type TypeicalT = StateT TypeicalState
type Typeical = TypeicalT Identity 

evalT :: Monad m => TypeicalT m a -> m a
evalT s = evalStateT s emptyState

runT :: Monad m => TypeicalT m a -> m (a, TypeicalState)
runT s = runStateT s emptyState

extendGramma :: Monad m => Gramma -> TypeicalT m ()
extendGramma g = modify (flip extend g) 

getGramma :: Monad m => TypeicalT m Gramma
getGramma = get

