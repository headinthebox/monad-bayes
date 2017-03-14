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
import Control.Monad.Bayes.Simple

newtype Validation m a = Validation {runValidation :: StateT (LogDomain (CustomReal m)) m a}
  deriving(Functor, Applicative, Monad, MonadIO)

instance MonadTrans Validation where
  lift = Validation . lift

instance HasCustomReal m => HasCustomReal (Validation m) where
  type CustomReal (Validation m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (Validation m) where
  sample = lift . sample

instance (Conditionable m, Monad m) => Conditionable (Validation m) where
  factor = lift . factor

instance MonadDist m => MonadDist (Validation m)
instance MonadBayes m => MonadBayes (Validation m)

hoist :: (CustomReal m ~ CustomReal n) => (forall x. m x -> n x)
      -> Validation m a -> Validation n a
hoist f (Validation m) = Validation (mapStateT f m)

validationScore :: (HasCustomReal m, Monad m) => LogDomain (CustomReal m) -> Validation m ()
validationScore w = Validation $ modify (* w)

validate :: (HasCustomReal m, Monad m) => Validation m a -> m (LogDomain (CustomReal m))
validate (Validation m) = execStateT m 1
