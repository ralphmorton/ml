{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ML.Vec where

import ML.Nat

import Control.Applicative ((<$>))

data Vec (a :: *) (n :: Nat) where
    Nil :: Vec a Z
    (:-) :: a -> Vec a n -> Vec a (S n)

instance Show a => Show (Vec a n) where
    show Nil = "[]"
    show (a :- r) = show a ++ " :- " ++ show r

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

mul :: Num a => a -> Vec a n -> Vec a n
mul s Nil = Nil
mul s (x :- xs) = (s * x) :- mul s xs

sub :: Num a => Vec a n -> Vec a n -> Vec a n
sub Nil Nil = Nil
sub (x :- xs) (y :- ys) = (x - y) :- sub xs ys
