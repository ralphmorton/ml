{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module ML.Vec where

import ML.Nat

import Control.Applicative ((<$>))

data Vec (n :: Nat) (a :: *) where
    Nil :: Vec Z a
    (:-) :: a -> Vec n a -> Vec (S n) a

instance Functor (Vec n) where
    fmap f Nil = Nil
    fmap f (x :- xs) = f x :- fmap f xs

instance Foldable (Vec n) where
    foldr f s Nil = s
    foldr f s (x :- xs) = foldr f (f x s) xs

instance Show a => Show (Vec n a) where
    show Nil = "[]"
    show (a :- r) = show a ++ " :- " ++ show r

infixr 5 :-

fromList :: SNat n -> [a] -> Maybe (Vec n a)
fromList SZ _ = Just Nil
fromList (SS n) (x:xs) = (x :-) <$> fromList n xs
fromList _ _ = Nothing

toList :: Vec n a -> [a]
toList Nil = []
toList (x :- xs) = x:(toList xs)

vzip :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vzip _ Nil _ = Nil
vzip f (x :- xs) (y :- ys) = f x y :- vzip f xs ys

vlen :: Vec n a -> Int
vlen = foldr (\_ x -> x + 1) 0

vdot :: Num a => Vec n a -> Vec n a -> a
vdot v1 v2 = vsum $ vzip (*) v1 v2

vmul :: Num a => a -> Vec n a -> Vec n a
vmul a = fmap (*a)

vsub :: Num a => Vec n a -> Vec n a -> Vec n a
vsub = vzip (-)

vsum :: Num a => Vec n a -> a
vsum = foldr (+) 0

vmax :: Ord a => Vec (S n) a -> a
vmax (x :- xs) = foldr (max) x xs
