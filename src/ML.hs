{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ML where

import ML.Nat
import ML.Vec

type family VectorFunc (n :: Nat) (c :: *) :: *
type instance VectorFunc Z c = c
type instance VectorFunc (S n) c = Double -> VectorFunc n c

classification :: SNat n -> VectorFunc n c -> Vec Double n -> c
classification SZ r _ = r
classification (SS n) rf (x:-xs) = classification n (rf x) xs

regression :: SNat n -> VectorFunc n Double -> Vec Double n -> Double
regression = classification
