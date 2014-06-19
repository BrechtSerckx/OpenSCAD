{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances,	FunctionalDependencies #-}

{- |
Module      : Graphics.OpenSCAD
Description : Type-checked wrappers for the OpenSCAD primitives.
Copyright   : &#xa9; Mike Meyer, 2014
License     : BSD4
Maintainer  : mwm@mired.org
Stability   : experimental

The Graphics.OpenSCAD module provides abstract data types for creating
OpenSCAD model definitions calls, along with a function to render it
as a string, and some utilities. The primary goal is that the output
should always be valid OpenSCAD. If you manage to generate OpenSCAD
source that causes OpenSCAD to complain, please open an issue.

Standard usage is to have a @main@ function that looks like:

@
main = draw $ /Solid/
@
or
@
main = drawL $ [/Solid/]
@

and then set your IDE's compile command to use @runhaskell@ or
equivalent to run your code and send the output to a .scad file. Open
that file in OpenSCAD, and set it to automatically reload if the file
changes. Recompiling your program will cause the model to be loaded
and displayed by OpenSCAD.

The type constructors are generally not exported, with functions being
exported in their stead.  This allows extra checking to be done on
those that need it.  It also provides consistency, as otherwise you'd
have to remember whether 'box' is a constructor or a convenience
function, etc.

Because of this, the constructors are not documented, the exported
functions are. The documentation is generally just the corresponding
OpenSCAD function name, along with the names of the arguments from the
OpenSCAD documentation. If no OpenSCAD function name is given, then
it's the same as the 'Graphics.OpenSCAD' function. You should check
the OpenSCAD documentation for usage information.

-}

module Graphics.OpenSCAD (
  -- * Basic data types
  Solid, Shape, Facet,
  -- * Type aliases for vectors, should you want them
  Vector2d, Vector3d,
  -- * Rendering functions
  render, renderL,
  -- * Constructors
  sphere, box, cube, cylinder, obCylinder, importFile, solid,
  linearExtrude, rotateExtrude, rectangle, square, circle, projection,
 -- * Combinations
  union, intersection, difference, minkowski, hull,
  -- * Transformations
  scale, resize, rotate, translate, mirror, multMatrix, color, transparent, up,
  -- * General convenience functions
  diam, draw, drawL,
  -- * Convenience functions for 'Facet's.
  var, fn, fs, fa, def,
  module Colours)

where

import Data.Colour (Colour, AlphaColour, alphaChannel, darken, over, black)
import Data.Colour.SRGB (channelRed, channelBlue, channelGreen, toSRGB)
import System.FilePath (FilePath)
import Data.Colour.Names as Colours

-- A vector in 2 or 3-space.
class Vector a where
  rVector :: a -> String

-- | A 'Model' is an object that can have be either 2d or 3d. This class
-- provides methods for creating them.
class Model a where
  -- | Create a rectangular 'Model' with @rectangle /x-size y-size/@.
  rectangle :: Float -> Float -> a
  -- | 'square' is a 'rectangle' with both sides the same size.
  square :: Float -> a
  square s = rectangle s s
  -- | Create a circular 'Model' with @circle /radius/ 'Facet'@.
  circle :: Float -> Facet -> a
  -- | Project a 'Solid' into a 'Model' with @projection /cut 'Solid'/@.
  projection :: Bool -> Solid -> a
  -- | __UNTESTED__ 'importFile' is @import /filename/@.
  importFile :: FilePath -> a
  -- Internal rendering of a Model.
  rModel :: a -> String

-- | A 'Transform' turns a 'Model' into another 'Model', usually using
-- a vector with the appropriate number of dimensions.
class (Model m, Vector v) => Transform m v | m -> v where
  -- | Scale a 'Model', the vector specifying the scale factor for each axis.
  scale :: v -> m -> m
  -- | __UNTESTED__ Resize a 'Model' to occupy the dimensions given by
  -- the vector.
  resize :: v -> m -> m
  -- | Rotate a 'Model' by different amounts around each of the three axis.
  rotate :: v -> m -> m
  -- | Translate a 'Model' along a 'Vector'.
  translate :: v -> m -> m
  -- | Mirror a 'Model' across a plane intersecting the origin.
  mirror :: v -> m -> m
  -- | Render a 'Model' in a specific color. This doesn't us the
  -- OpenSCAD color model, but instead uses the 'Data.Colour' model. The
  -- 'Graphics.OpenSCAD' module rexports 'Data.Colour.Names' so you can
  -- conveniently say @'color' 'red' /'Solid'/@.
  color :: Colour Float -> m -> m
  -- | Render a 'Solid' in a transparent color. This uses the
  -- 'Data.Coulor.AphaColour' color model.
  transparent :: AlphaColour Float -> m -> m
  -- | Create the union of a list of 'Solid's.
  union :: [m] -> m
  -- | Create the intersection of a list of 'Models's.
  intersection :: [m] -> m
  -- | The difference between two 'Model's.
  difference :: m -> m -> m
  -- | The Minkowski sum of a list of 'Solid's.
  minkowski :: [m] -> m
  -- | The convex hull of a list of 'Solid's.
  hull :: [m] -> m


-- | 'Vector2d' is used where OpenSCAD expects an OpenSCAD @vector@ of length 2.
type Vector2d = (Float, Float)
instance Vector Vector2d where
  rVector (x, y) = "[" ++ show x ++ "," ++ show y ++ "]"

-- | 'Vector3d' is used where OpenSCAD expects an OpenSCAD @vector@ of length 3.
type Vector3d = (Float, Float, Float)
instance Vector Vector3d where
  rVector (a, b, c) = "[" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "]"

-- These are for @Poly*s@, which don't work yet.
type Path = [Int]
type Face = (Int, Int, Int)

-- a 4x4 transformation matrix specifying a complete 3-space transform. 
type TransMatrix = ((Float, Float, Float, Float), (Float, Float, Float, Float),
                    (Float, Float, Float, Float), (Float, Float, Float, Float))


-- While it's tempting to add more options to Solid, don't do it. Instead,
-- add functions that add that functionality, like cube vs. box.
--
-- Missing at this time: Poly*s, some special features.
-- There's also no way to set $f? vars globally, due to an OpenSCAD quirk.

-- | A 'Facet' is used to set one of the special variables that
-- control the mesh used during generation of circular objects. They
-- appear as arguments to various constructors, as well as in the
-- 'var' function to set them for the argument objects.
data Facet = Fa Float | Fs Float | Fn Int | Def deriving Show

-- | A 'Shape' is a two-dimensional object. They are a separate type
-- so that Haskell can type check that we aren't using a 2d operation
-- on a 3d shape, or vice versa.
data Shape =
             Rectangle Float Float
           | Circle Float Facet
           -- add | Polygon [Vector2d] [Path] Int
           | Projection Bool Solid
           | Import2d FilePath
           -- 2d versions of the transformations
           | Scale2d Vector2d Shape
           | Resize2d Vector2d Shape
           | Rotate2d Vector2d Shape
           | Translate2d Vector2d Shape
           | Mirror2d Vector2d Shape
           | Color2d (Colour Float) Shape
           | Transparent2d (AlphaColour Float) Shape
           -- and the combinations
           | Union2d [Shape]
           | Intersection2d [Shape]
           | Difference2d Shape Shape
           | Minkowski2d [Shape]
           | Hull2d [Shape]
           deriving Show

instance Model Shape where
  rectangle w d = Rectangle w d
  circle = Circle
  projection = Projection
  importFile = Import2d
  rModel = rShape

instance Transform Shape Vector2d where 
  scale = Scale2d
  resize = Resize2d
  rotate = Rotate2d
  translate = Translate2d
  mirror = Mirror2d
  color = Color2d
  transparent = Transparent2d
  union = Union2d
  intersection = Intersection2d
  difference = Difference2d
  minkowski = Minkowski2d
  hull = Hull2d


-- | A 'Solid' is a solid object in OpenSCAD. Since we don't have
-- optional or named objects, some constructors appear twice to allow
-- two different variants to be used. And of course, they all have all
-- their arguments.
data Solid = Sphere Float Facet
           | Box Float Float Float
           | Cylinder Float Float Facet
           | ObCylinder Float Float Float Facet
           -- add | Polyhedron [Vector3d] [Face] Int
           | Import3d FilePath
           | Solid Shape
           -- Combinations
           | Union [Solid]
           | Intersection [Solid]
           | Difference Solid Solid
           | Minkowski [Solid]
           | Hull [Solid]
           -- Transformations
           | Scale Vector3d Solid
           | Resize Vector3d Solid
           | Rotate Vector3d Solid
           | Translate Vector3d Solid
           | Mirror Vector3d Solid
           | MultMatrix TransMatrix Solid
           | Color (Colour Float) Solid
           | Transparent (AlphaColour Float) Solid
           | LinearExtrude Float Float Vector2d Int Int Facet Shape
           | RotateExtrude Int Facet Shape
           -- Mesh control
           | Var Facet [Solid]
           deriving Show

instance Model Solid where
  rectangle w d = Solid $ Rectangle w d
  circle r f = Solid $ Circle r f
  projection c s = Solid $ Projection c s
  importFile = Import3d
  rModel = render

instance Transform Solid Vector3d where 
  scale = Scale
  resize = Resize
  rotate = Rotate
  translate = Translate
  mirror = Mirror
  color = Color
  transparent = Transparent
  union = Union
  intersection = Intersection
  difference = Difference
  minkowski = Minkowski
  hull = Hull


-- | 'render' does all the real work. It will walk the AST for a 'Solid',
-- returning an OpenSCAD program in a 'String'.
render :: Solid -> String
render (Sphere x f) = "sphere(" ++ show x ++ rFacet f ++ ");\n\n"
render (Box x y z) =
  "cube([" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]);\n"
render (Cylinder r h f) =
  "cylinder(r=" ++ show r ++ ",h=" ++ show h ++ rFacet f ++ ");\n\n"
render (ObCylinder r1 h r2 f) =
    "cylinder(r1=" ++ show r1 ++ ",h=" ++ show h ++ ",r2=" ++ show r2 ++ rFacet f
    ++ ");\n\n"
render (Import3d f) = "import(" ++ f ++");\n\n"
render (Solid s) = rModel s
render (Union ss) = rList "union()" ss
render (Intersection ss) = rList "intersection()" ss
render (Difference s1 s2) = "difference(){" ++ rModel s1 ++ rModel s2 ++ "}\n\n"
render (Minkowski ss) = rList "minkowski()" ss
render (Hull ss) = rList "hull()" ss
render (Scale v s) = rVecSolid "scale" v s
render (Resize v s) = rVecSolid "resize" v s
render (Translate v s) = rVecSolid "translate" v s
render (Rotate v s) = "rotate(" ++ rVector v ++ ")" ++ rModel s
render (Mirror v s) = rVecSolid "mirror" v s
render (MultMatrix (a, b, c, d) s) =
    "multmatrix([" ++ rQuad a ++ "," ++ rQuad b ++ "," ++ rQuad c ++ ","
    ++ rQuad d ++"])\n" ++ rModel s
render (Color c s) = let r = toSRGB c in
    "color(" ++ rVector (channelRed r, channelGreen r, channelBlue r) ++ ")\n"
    ++ rModel s
render (Transparent c s) =
    "color(" ++ rQuad (channelRed r, channelGreen r, channelBlue r, a) ++ ")"
    ++ rModel s
    where r = toSRGB $ toPure c
          a = alphaChannel c
          toPure ac = if a > 0 then darken (recip a) (ac `over` black) else black
render (LinearExtrude h t sc sl c f sh) =
    "linear_extrude(height=" ++ show h ++ ",twist=" ++ show t ++ ",scale="
    ++ rVector sc ++ ",slices=" ++ show sl ++ ",convexity=" ++ show c
    ++ rFacet f ++ ")" ++ rModel sh
render (RotateExtrude c f sh) =
  "rotate_extrude(convexity=" ++ show c ++ rFacet f ++ ")" ++ rModel sh
render (Var (Fa f) ss) = rList ("assign($fa=" ++ show f ++ ")") ss
render (Var (Fs f) ss) = rList ("assign($fs=" ++ show f ++ ")") ss
render (Var (Fn n) ss) = rList ("assign($fn=" ++ show n ++ ")") ss

-- | A convenience function to render a list of 'Solid's by taking
-- their union.
renderL :: [Solid] -> String
renderL = render . union

-- | A convenience function to write the rendered 'Solid' to
-- standard output.
draw :: Solid -> IO ()
draw = putStrLn . render

-- | A convenience function to write a 'union' of 'Solid's to
-- standard output.
drawL :: [Solid] -> IO ()
drawL = draw . Union

-- utilities for rendering Shapes.
rShape (Rectangle r f) = "square([" ++ show r ++ "," ++ show f ++ "]);\n\n"
rShape (Circle r f) = "circle(" ++ show r ++ rFacet f ++ ");\n\n"
rShape (Import2d f) = "import(" ++ f ++ ");\n\n"
rShape (Projection c s) =
  "projection(cut=" ++ (if c then "true)" else "false)") ++ rModel s
rShape (Scale2d p s) = "scale(" ++ rVector p ++ ")" ++ rModel s
rShape (Resize2d p s) = "resize(" ++ rVector p ++ ")" ++ rModel s
rShape (Rotate2d p s) = "rotate(" ++ rVector p ++ ")" ++ rModel s
rShape (Translate2d p s) = "translate(" ++ rVector p ++ ")" ++ rModel s
rShape (Mirror2d p s) = "mirror(" ++ rVector p ++ ")" ++ rModel s
rShape (Color2d c s) = let r = toSRGB c in
    "color(" ++ rVector (channelRed r, channelGreen r, channelBlue r) ++ ")\n"
    ++ rModel s
rShape (Transparent2d c s) =
    "color(" ++ rQuad (channelRed r, channelGreen r, channelBlue r, a) ++ ")"
    ++ rModel s
    where r = toSRGB $ toPure c
          a = alphaChannel c
          toPure ac = if a > 0 then darken (recip a) (ac `over` black) else black


-- And some misc. rendering utilities.
rList n ss = n ++ "{\n" ++  concatMap rModel ss ++ "}"
rSolid n s = n ++ "()\n" ++ rModel s
rVecSolid n v s = n ++ "(" ++ rVector v ++ ")\n" ++ rModel s
rQuad (w, x, y, z) =
  "[" ++ show w ++ "," ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
rFacet Def = ""
rFacet f = "," ++ showFacet f

-- render a facet setting.
showFacet :: Facet -> String
showFacet (Fa f) = "$fa=" ++ show f
showFacet (Fs f) = "$fs=" ++ show f
showFacet (Fn n) = "$fn=" ++ show n
showFacet Def    = ""

-- | Create a sphere with @sphere /radius 'Facet'/@.
sphere :: Float -> Facet -> Solid
sphere = Sphere

-- | Create a box with @cube /x-size y-size z-size/@
box :: Float -> Float -> Float -> Solid
box = Box

-- | A convenience function for creating a cube as a 'box' with all
-- sides the same length.
cube :: Float -> Solid
cube x = Box x x x

-- | Create a cylinder with @cylinder /radius height 'Facet'/@.
cylinder :: Float -> Float -> Facet -> Solid
cylinder = Cylinder

-- | Create an oblique cylinder with @cylinder /radius1 height radius2
-- 'Facet'/@
obCylinder :: Float -> Float -> Float -> Facet -> Solid
obCylinder = ObCylinder

-- | Transform a 'Solid' with a 'TransMatrix'
multMatrix :: TransMatrix -> Solid -> Solid
multMatrix = MultMatrix

-- | A 'translate' that just goes up, since those seem to be common.
up :: Float -> Solid -> Solid
up f = Translate (0, 0, f)


-- Tools to add a dimensions.
-- | Turn a 'Shape' into a 'Solid' exactly as is.
solid :: Shape -> Solid
solid = Solid

-- | Extrude a 'Shape' along a line with @linear_extrude@.
linearExtrude :: Float         -- ^ height
              -> Float         -- ^ twist
              -> Vector2d      -- ^ scale
              -> Int           -- ^ slices
              -> Int           -- ^ convexity
              -> Facet
              -> Shape         -- ^ to extrude
              -> Solid
linearExtrude = LinearExtrude

-- | Rotate a 'Shape' around the origin with @rotate_extrude
-- /convexity 'Facet' 'Shape'/@
rotateExtrude ::  Int -> Facet -> Shape -> Solid
rotateExtrude = RotateExtrude

-- Convenience functions for Facets.
-- Maybe this should have type [Facet] -> [Solid] -> [Solid]
-- | 'var' uses @assign@ to set a special variable for the 'Solid's.
var :: Facet -> [Solid] -> Solid
var = Var

-- | 'fa' is used to set the @$fa@ variable in a 'Facet' or 'var'.
fa :: Float -> Facet
fa = Fa

-- | 'fs' is used to set the @$fs@ variable in a 'Facet' or 'var'.
fs :: Float -> Facet
fs = Fs

-- | 'fn' is used to set the @$fn@ variable in a 'Facet' or 'var'.
fn :: Int -> Facet
fn = Fn

-- | 'def' is used where a 'Facet' is needed but we don't want to change
-- any of the values.
def :: Facet
def = Def

-- And one last tool.
-- | Use 'diam' to turn a diameter into a radius for circles, spheres, etc.
diam :: Float -> Float
diam = (/ 2)

