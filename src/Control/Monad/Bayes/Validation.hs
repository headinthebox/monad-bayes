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
    toWeighted,
    fromWeighted,
    runValidation,
    withValidation,
    hoist,
    validationScore,
    validate
                ) where

import Control.Monad.Trans

import Control.Monad.Bayes.LogDomain
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Simple
import Control.Monad.Bayes.Weighted hiding (hoist)
import qualified Control.Monad.Bayes.Weighted as W

newtype Validation m a = Validation (Weighted m a)
  deriving(Functor, Applicative, Monad, MonadIO, MonadTrans)

toWeighted :: Validation m a -> Weighted m a
toWeighted (Validation m) = m

fromWeighted :: Weighted m a -> Validation m a
fromWeighted = Validation

runValidation :: Validation m a -> m (a, LogDomain (CustomReal m))
runValidation = runWeighted . toWeighted

withValidation :: m (a, LogDomain (CustomReal m)) -> Validation m a
withValidation = fromWeighted . withValidation

instance HasCustomReal m => HasCustomReal (Validation m) where
  type CustomReal (Validation m) = CustomReal m

instance (Sampleable d m, Monad m) => Sampleable d (Validation m) where
  sample = lift . sample

instance (Conditionable m, Monad m) => Conditionable (Validation m) where
  factor = lift . factor

instance MonadDist m => MonadDist (Validation m)
instance MonadBayes m => MonadBayes (Validation m)

hoist :: (forall x. m x -> m x)
      -> Validation m a -> Validation m a
hoist f (Validation m) = Validation (W.hoist f m)

validationScore :: (HasCustomReal m, Monad m) => LogDomain (CustomReal m) -> Validation m ()
validationScore w = Validation $ factor w

validate :: (HasCustomReal m, MonadDist m) => Validation m a -> m (LogDomain (CustomReal m))
validate (Validation m) = fmap snd $ runWeighted m
