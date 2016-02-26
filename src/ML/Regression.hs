{-# LANGUAGE DataKinds #-}

module ML.Regression(
    linearRegression
) where

import ML.Nat
import ML.Vec

linearRegression :: Num a => Vec a n -> Vec a n -> a
linearRegression = dot
