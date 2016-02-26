
module ML.GradientDescent(
    train
) where

import ML.Nat
import ML.Vec

import Data.Maybe (fromJust)

train :: (Floating a, Ord a) => SNat n -> a -> a -> (Vec n a -> Vec n a -> a) -> [(Vec n a, a)] -> Vec n a -> Vec n a
train sn alpha epsilon hypf tdata theta
    | err < epsilon = theta'
    | otherwise = train sn epsilon alpha hypf tdata theta'
    where
    grads = mseGrads sn (hypf theta) tdata
    theta' = vsub theta $ vmul alpha grads
    err = abs $ vsum grads / fromIntegral (vlen grads)

mse :: Floating a => (Vec n a -> a) -> [(Vec n a, a)] -> a
mse _ [] = fromIntegral 0
mse hyp dx = (* coeff) . sum . fmap cost $ dx
    where
    coeff = 1 / (fromIntegral $ 2 * length dx)
    cost (ix, y) = (hyp ix - y) ** 2

mseGrads :: Floating a => SNat n -> (Vec n a -> a) -> [(Vec n a, a)] -> Vec n a --TODO: make training set an n+1 vec
mseGrads sn hyp dx = fromJust . fromList sn . fmap (/ (fromIntegral $ length dx)) . foldl1 (zipWith (+)) . fmap cost' $ dx
    where
    cost' (ix, y) = zipWith (*) ixl (replicate (length ixl) hres)
        where
        ixl = toList ix
        hres = hyp ix - y
