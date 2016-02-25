{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module ML where

import ML.Nat
import ML.Vec

type family VectorFunc (n :: Nat) (a :: *) (c :: *) :: *
type instance VectorFunc n a c = Vec a n -> c

hypothesis :: VectorFunc n a c -> Vec a n -> c
hypothesis h ix = h ix
