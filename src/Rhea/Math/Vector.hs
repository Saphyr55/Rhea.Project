{-# LANGUAGE InstanceSigs #-}

module Rhea.Math.Vector 
  (
    Vector3 (..), Vector3f, Vector3i,
    Vector4 (..), Vector4f, Vector4i,
    Vector (..)
  ) where

data Vector4 a = Vector4 !a !a !a !a
type Vector4f = Vector4 Float
type Vector4i = Vector4 Int

data Vector3 a = Vector3 !a !a !a
type Vector3f = Vector3 Float
type Vector3i = Vector3 Int

class Vector t where

  norm :: (Real a) => t a -> Float

  (!) :: (Real a) => t a -> Int -> a

  head :: (Real a) => t a -> a

  last :: (Real a) => t a -> a

  (.) :: (Real a) => t a -> t a -> Float

instance Vector Vector4 where

  norm :: (Real a) => Vector4 a -> Float
  norm (Vector4 a1 a2 a3 a4) = sqrt
      $! realToFrac
      $ a1 * a1 + a2 * a2 + a3 * a3 + a4 * a4

  (!) :: Vector4 a -> Int -> a
  (!) (Vector4 a1 _ _ _) 0 = a1
  (!) (Vector4 _ a2 _ _) 1 = a2
  (!) (Vector4 _ _ a3 _) 2 = a3
  (!) (Vector4 _ _ _ a4) 3 = a4
  (!) _ _ = error "An index greater than three is not permitted."

  head :: Vector4 a -> a
  head (Vector4 a1 _ _ _) = a1

  last :: Vector4 a -> a
  last (Vector4 _ _ _ a) = a

  (.) :: (Real a) => Vector4 a -> Vector4 a -> Float
  (.) (Vector4 a11 a12 a13 a14) (Vector4 a21 a22 a23 a24) =
    dotProductList [a11, a12, a13, a14] [a21, a22, a23, a24]

instance Vector Vector3 where

  norm :: (Real a) => Vector3 a -> Float
  norm (Vector3 a1 a2 a3) = sqrt
    $! realToFrac
    $ a1 * a1 + a2 * a2 + a3 * a3

  (!) :: Vector3 a -> Int -> a
  (!) (Vector3 a1 _ _) 0 = a1
  (!) (Vector3 _ a2 _) 1 = a2
  (!) (Vector3 _ _ a3) 2 = a3
  (!) _ _ = error "An index greater than two is not permitted."

  head :: Vector3 a -> a
  head (Vector3 a1 _ _) = a1

  last :: Vector3 a -> a
  last (Vector3 _ _ a) = a

  (.) :: (Real a) => Vector3 a -> Vector3 a -> Float
  (.) (Vector3 a11 a12 a13) (Vector3 a21 a22 a23) =
    dotProductList [a11, a12, a13] [a21, a22, a23]

sum'aux :: (Real r) => (a -> r) -> r -> [a] -> r
sum'aux _ acc [] = acc
sum'aux f acc (t:h) = sum'aux f (acc + f t) h

sum' :: (Real r) => (a -> r) -> [a] -> r
sum' f = sum'aux f 0

dotProductList :: Real r => [r] -> [r] -> Float
dotProductList l1 l2 =
  let l = zip l1 l2 in
  let x = sum' (uncurry (*)) l in
  realToFrac x

