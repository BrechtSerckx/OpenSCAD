{-# LANGUAGE UnicodeSyntax #-}

{-
Module      : Graphics.OpenSCAD.Unicode
Description : Unicode operators so you can write 'Model' expressions.
Copyright   : &#xa9; Mike Meyer, 2014
License     : BSD4
Maintainer  : mwm@mired.org
Stability   : experimental
-}

module Graphics.OpenSCAD.Unicode where

import Data.Monoid ((<>))
import Graphics.OpenSCAD

infixl 6 ∪
infixr 6 ∩
infixl 9 ∖
infixl 9 ⊖
infixl 9 ⊕

-- | (&#x222A;) = 'union'
--
-- U+222A, UNION
(∪) :: Vector v => Model v -> Model v -> Model v
(∪) = (<>)

-- | (&#x2229;) = 'intersection'
--
-- U+2229, INTERSECTION
(∩) :: Vector v => Model v -> Model v -> Model v
a ∩ b = intersection [a, b]

-- | (&#x2216;) = 'difference'
--
-- U+2216, SET MINUS
(∖):: Vector v => Model v -> Model v -> Model v
(∖) = difference

-- | (&#x2296;) = Symmetric difference
--
-- U+2296, CIRCLED MINUS
(⊖) :: Vector v => Model v -> Model v -> Model v
a ⊖ b = (a ∖ b) ∪ (b ∖ a)

-- | (&#2295;) = 'minkowski'
--
-- U+2295, CIRCLED PLUS
(⊕) :: Vector v => Model v -> Model v -> Model v
a ⊕ b = minkowski [a, b]
