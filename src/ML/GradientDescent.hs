
module ML.GradientDescent(
    train
) where

import ML.Nat
import ML.Vec

import Data.Maybe (fromJust)

train :: (Floating a, Ord a) => SNat n -> a -> a -> (Vec a n -> Vec a n -> a) -> [(Vec a n, a)] -> Vec a n -> Vec a n
train sn alpha epsilon hypf tdata theta
    | err < epsilon = theta'
    | otherwise = train sn epsilon alpha hypf tdata theta'
    where
    grads = mseGrads sn (hypf theta) tdata
    gradsl = toList grads
    theta' = sub theta $ mul alpha grads
    err = abs $ sum gradsl / fromIntegral (length gradsl)

mse :: Floating a => (Vec a n -> a) -> [(Vec a n, a)] -> a
mse _ [] = fromIntegral 0
mse hyp dx = (* coeff) . sum . fmap cost $ dx
    where
    coeff = 1 / (fromIntegral $ 2 * length dx)
    cost (ix, y) = (hyp ix - y) ** 2

mseGrads :: Floating a => SNat n -> (Vec a n -> a) -> [(Vec a n, a)] -> Vec a n --TODO: make training set an n+1 vec
mseGrads sn hyp dx = fromJust . fromList sn . fmap (/ (fromIntegral $ length dx)) . foldl1 (zipWith (+)) . fmap cost' $ dx
    where
    cost' (ix, y) = zipWith (*) ixl (replicate (length ixl) hres)
        where
        ixl = toList ix
        hres = hyp ix - y
