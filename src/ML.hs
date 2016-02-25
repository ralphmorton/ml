{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ML where

import ML.Nat
import ML.Vec

type family VectorFunc (n :: Nat) (a :: *) (c :: *) :: *
type instance VectorFunc Z a c = c
type instance VectorFunc (S n) a c = a -> VectorFunc n a c

hypothesis :: SNat n -> VectorFunc n a c -> Vec a n -> c
hypothesis SZ r _ = r
hypothesis (SS n) rf (x:-xs) = hypothesis n (rf x) xs
