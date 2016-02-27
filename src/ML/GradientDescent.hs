{-# LANGUAGE DataKinds #-}

module ML.GradientDescent(
    train
) where

import ML.Nat
import ML.Vec

import Data.Maybe (fromJust)

train :: (Floating a, Ord a) => SNat (S n) -> a -> a -> (Vec (S n) a -> Vec (S n) a -> a) -> [(Vec (S n) a, a)] -> Vec (S n) a -> Vec (S n) a
train sn alpha epsilon hypf tdata theta
    | err < epsilon = theta'
    | otherwise = train sn epsilon alpha hypf tdata theta'
    where
    grads = mseGrads sn (hypf theta) tdata
    theta' = vsub theta $ vmul alpha grads
    err = abs $ vmax grads

mse :: Floating a => (Vec (S n) a -> a) -> [(Vec (S n) a, a)] -> a
mse _ [] = fromIntegral 0
mse hyp dx = (* coeff) . sum . fmap cost $ dx
    where
    coeff = 1 / (fromIntegral $ 2 * length dx)
    cost (ix, y) = (hyp ix - y) ** 2

mseGrads :: Floating a => SNat (S n) -> (Vec (S n) a -> a) -> [(Vec (S n) a, a)] -> Vec (S n) a --TODO: make training set an n+1 vec
mseGrads sn hyp dx = fromJust . fromList sn . fmap (/ (fromIntegral $ length dx)) . foldl1 (zipWith (+)) . fmap cost' $ dx
    where
    cost' (ix, y) = zipWith (*) ixl (replicate (length ixl) hres)
        where
        ixl = toList ix
        hres = hyp ix - y
