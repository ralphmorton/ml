{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ML.Vec where

import ML.Nat

import Control.Applicative ((<$>))

data Vec (a :: *) (n :: Nat) where
    Nil :: Vec a Z
    (:-) :: a -> Vec a n -> Vec a (S n)

infixr 5 :-

fromList :: SNat n -> [a] -> Maybe (Vec a n)
fromList SZ _ = Just Nil
fromList (SS n) (x:xs) = (x :-) <$> fromList n xs
fromList _ _ = Nothing

toList :: Vec a n -> [a]
toList Nil = []
toList (x :- xs) = x:(toList xs)

dot :: Num a => Vec a n -> Vec a n -> a
dot va vb = sum $ zipWith (*) ax bx
    where
    ax = toList va
    bx = toList vb
