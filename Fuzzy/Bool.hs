{-# LANGUAGE TypeFamilies, OverloadedLists, FlexibleInstances, FunctionalDependencies, ViewPatterns, MultiParamTypeClasses, UnicodeSyntax, GeneralizedNewtypeDeriving #-}
module Fuzzy.Bool where

import Prelude.Unicode
import Control.Applicative
import Data.Monoid
import GHC.Exts

class Adjunct f u | f → u where
    forget ∷ f → u
    construct ∷ u → f

newtype PolygonicNumber = Poly {
     unPoly ∷ [Double]
  }

instance Show PolygonicNumber where
     show (forget → [a,b,c]) = "Poly $ " <> show a <> " ← "  <> show b <> " → " <> show c

instance Adjunct PolygonicNumber [Double] where
     forget (Poly xs) = xs
     construct = Poly

instance IsList PolygonicNumber where
   type Item PolygonicNumber = Double
   fromList = Poly
   toList (Poly xs) = xs

instance Eq PolygonicNumber where
     (==) (forget → [al,bl,cl]) (forget → [ar, br, cr]) | ar ≡ al ∧ br ≡ ar ∧ cr ≡ cl = True
                                                        | otherwise = False

instance Ord PolygonicNumber where
     (<=) (forget → [al, bl, cl]) (forget → [ar, br, cr]) | bl ≡ br ∧ cl ≤ cr ∧ al ≤ ar = True
                                                          | otherwise = False



class PolyNumbers β where
    μ  ∷ β → Double → Double
    spreadleft ∷ β → Double
    spreadright :: β → Double

instance PolyNumbers PolygonicNumber where
    μ β@(forget → [a,b,c]) x | a ≤ x ∧ x ≤ b = (x - a) / spreadleft β
                             | b ≤ x ∧ x ≤ c = (c - x) / spreadright β
                             | otherwise = 0
    spreadleft (forget → [a,b,c]) = b - a
    spreadright (forget → [a,b,c]) = c - b

instance Fractional PolygonicNumber where
    fromRational a = Poly $ let b = fromRational a
                                       in [b,b,b]
    (/) (forget → xs) (forget → ys) = construct $ zipWith (/) xs ys

member ∷ PolyNumbers a ⇒ a → Double → Double
member = μ

-- | These types of fuzzy logics are all monoidal in nature
class  FuzzyLogic α  where
   -- | Laws
   --
   -- for τ-norm ∷ α → α → α
   -- with tunit ∷ α
   -- α ⊕ β = β ⊕ α
   -- α ⊕ β ≤ γ ⊕ δ if α ≤ γ and β ≤ δ
   -- α ⊕ (β ⊕ γ) = (α ⊕ β) ⊕ γ
   -- tunit ⊕ α = α
   -- α ⊕ tunit = α
   tnorm :: α -> α -> α
   tunit ∷ α
   -- | Forget the algebraic structure


instance Num PolygonicNumber where
       -- Creates a spike with exactly spread 0
       fromInteger x = construct $ fromInteger <$> [x,x,x]
       (forget → xs) + (forget → ys) = construct $ zipWith (+) xs ys
       (forget → xs) * (forget → ys) = construct $ zipWith (*) xs ys
       abs (forget → xs) = construct $ abs <$> xs
       negate (forget → xs) = construct $ reverse $ negate <$> xs
       signum (forget → xs) = construct $ signum <$> xs



fib ∷ (Eq a, Fractional a, Num a) ⇒ a → a
fib a | a ≡ 0 = 0
fib n = fib (n - 1) + fib (n - 2)
