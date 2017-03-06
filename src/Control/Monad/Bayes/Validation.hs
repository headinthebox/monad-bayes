{-|
Module      : Control.Monad.Bayes.Validation
Description : Validation scores for probabilistic programs
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Control.Monad.Bayes.Validation (
    Validation,
    runValidation,
    hoist,
    validationScore,
    validate
                ) where

import Control.Monad.Trans
import Control.Monad.Trans.State

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class

newtype Validation m a = Validation {runValidation :: StateT (LogDomain (CustomReal m)) m a}
  deriving(Functor, Applicative, Monad, MonadIO)

instance MonadTrans Validation where
  lift = Validation . lift

type instance CustomReal (Validation m) = CustomReal m

instance MonadDist m => MonadDist (Validation m) where
  primitive = lift . primitive

instance MonadBayes m => MonadBayes (Validation m) where
  factor  = lift . factor

hoist :: (CustomReal m ~ CustomReal n) => (forall x. m x -> n x)
      -> Validation m a -> Validation n a
hoist f (Validation m) = Validation (mapStateT f m)

validationScore :: MonadDist m => LogDomain (CustomReal m) -> Validation m ()
validationScore w = Validation $ modify (* w)

validate :: MonadDist m => Validation m a -> m (LogDomain (CustomReal m))
validate (Validation m) = execStateT m 1
