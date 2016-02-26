{-# LANGUAGE DataKinds #-}

module ML.Regression(
    linearRegression
) where

import ML.Nat
import ML.Vec

linearRegression :: Num a => Vec n a -> Vec n a -> a
linearRegression = vdot
