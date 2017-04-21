{-|
Module      : Numeric.Optimization.SGD
Description : Stochastic gradient descent
Copyright   : (c) Adam Scibior, 2017
License     : MIT
Maintainer  : ams240@cam.ac.uk
Stability   : experimental
Portability : GHC

-}

module Numeric.Optimization.SGD (
  SGDParam(SGDParam),
  learningRate,
  decayRate,
  steps,
  sga,
  sgd
) where

import Data.Bifunctor (second)

-- | Parameters for stochastic gradient descent.
data SGDParam r = SGDParam {learningRate :: r, decayRate :: r, steps :: Int}

-- | Checks SGD parameters, throws an error if any of them are invalid.
validateSGDParam :: SGDParam r -> SGDParam r
validateSGDParam p@(SGDParam _ _ s) = check `seq` p where
  check = if s < 0 then error "SGDParam: number of steps was negative" else ()

-- | Stochastic gradient ascent - finds a maximum of the given function.
sga :: (Monad m, Traversable t, Num r)
    => SGDParam r
    -> (t r -> m (t (r,r))) -- ^ stochastic function augmenting arguments with the gradient of the target
    -> t r -- ^ initial value of arguments
    -> m (t r)
sga param f xs0 = go (steps $ validateSGDParam param) (learningRate param) xs0 where
  go 0 _ xs = return xs
  go n r xs = do
    xs' <- step r xs
    go (n-1) (r * decayRate param) xs'

  step rate xs = do
    ys <- f xs
    return $ fmap (\(x, gx) -> x + rate * gx) ys

-- | Stochastic gradient descent - finds a minimum by running 'sga' on a function with negated output.
sgd :: (Monad m, Traversable t, Num r)
    => SGDParam r
    -> (t r -> m (t (r,r))) -- ^ stochastic function augmenting arguments with the gradient of the target
    -> t r -- ^ initial value of arguments
    -> m (t r)
sgd param f = sga (validateSGDParam param) (fmap (fmap (second negate)) . f)
