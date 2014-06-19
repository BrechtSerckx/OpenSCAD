{-# LANGUAGE UnicodeSyntax #-}

{-
Module      : Graphics.OpenSCAD.Unicode
Description : Unicode operators so you can write @Solid@ expressions.
Copyright   : &#xa9; Mike Meyer, 2014
License     : BSD4
Maintainer  : mwm@mired.org
Stability   : experimental
-}

module Graphics.OpenSCAD.Unicode where

import Graphics.OpenSCAD

infixl 6 ∪
infixr 6 ∩
infixl 9 ∖
infixl 9 ∆

-- | (&#x222A;) = 'union'
--
-- U+222A, UNION
(∪) :: Solid -> Solid -> Solid
a ∪ b = union [a, b]

-- | (&#x2229;) = 'intersection'
--
-- U+2229, INTERSECTION
(∩) :: Solid -> Solid -> Solid
a ∩ b = intersection [a, b]

-- | (&#x2216;) = 'difference'
--
-- U+2216, SET MINUS
(∖):: Solid -> Solid -> Solid
(∖) = difference

-- | (&#x2206;) = Symmetric difference
--
-- U+2206, INCREMENT
(∆) :: Solid -> Solid -> Solid
a ∆ b = (a ∖ b) ∪ (b ∖ a)

