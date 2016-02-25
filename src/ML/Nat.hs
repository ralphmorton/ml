{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module ML.Nat where

import Numeric.Natural

data Nat = Z | S Nat

data SNat n where
    SZ :: SNat Z
    SS :: SNat n -> SNat (S n)

s0 = SZ
s1 = SS s0
s2 = SS s1
s3 = SS s2
s4 = SS s3
s5 = SS s4
s6 = SS s5
s7 = SS s6
s8 = SS s7
s9 = SS s8
s10 = SS s9
