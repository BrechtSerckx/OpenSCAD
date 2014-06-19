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
  -- * Type aliases to save typing
  Vector, Point,
  -- * Rendering functions
  render, renderL,
  -- * Constructors
  -- ** 'Solid's
  sphere, box, cube, cylinder, obCylinder, rectangle3d, square3d, circle3d,
  import3d, linearExtrude, rotateExtrude,
  -- ** 'Shape's
  rectangle, square, circle, import2d, projection,
 -- * Combinations of 'Solid's
  union, intersection, difference, minkowski, hull,
  -- * Transformations
  -- ** 'Solid's
  scale, resize, rotate, translate, mirror, multMatrix, color, transparent, up,
  projection3d, 
  -- ** 'Shape's
  scale2d, resize2d, rotate2d, translate2d, mirror2d,
  -- ** General convenience functions
  diam, draw, drawL,
  -- * Convenience functions for 'Facet's.
  var, fn, fs, fa, def,
  module Colours)

where

import Data.Colour (Colour, AlphaColour, alphaChannel, darken, over, black)
import Data.Colour.SRGB (channelRed, channelBlue, channelGreen, toSRGB)
import System.FilePath (FilePath)
import Data.Colour.Names as Colours

-- | 'Vector' is used where OpenSCAD expects an OpenSCAD @vector@ of length 3.
type Vector = (Float, Float, Float)

-- | 'Point' is used where OpenSCAD expects an OpenSCAD @vector@ of length 3.
type Point = (Float, Float)

-- | These are for @Poly*s@, which don't work yet.
type Path = [Int]
type Face = (Int, Int, Int)

type Transform = ((Float, Float, Float, Float), (Float, Float, Float, Float),
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
-- on a 3d shape, or vice versa. Unfortunately, this means the
-- dynamically typed functions that accept either - and possibly
-- generate either - need to have two versions of those functions.  I
-- believe the 2d creation functions are more common for 2d objects,
-- but the 3d transformation functions are more common. Hence the 2d
-- creation functions (which have the names of 2d objects like circle,
-- square, etc.) that create 'Solid's have @3d@ appended, but the 3d
-- version of transformations that have both 2d and 3d versions have
-- @3d@ appended.
data Shape =
             Rectangle Float Float
           | Circle Float Facet
           -- add | Polygon [Point] [Path] Int
           | Import2d FilePath
           | Projection Bool Solid
           -- 2d versions of the transformations
           | Scale2d Point Shape
           | Resize2d Point Shape
           | Rotate2d Point Shape
           | Translate2d Point Shape
           | Mirror2d Point Shape
           deriving Show

-- | A 'Solid' is a solid object in OpenSCAD. Since we don't have
-- optional or named objects, some constructors appear twice to allow
-- two different variants to be used. And of course, they all have all
-- their arguments.
data Solid =
             Sphere Float Facet
           | Box Float Float Float
           | Cylinder Float Float Facet
           | ObCylinder Float Float Float Facet
           -- add | Polyhedron [Vector] [Face] Int
           | Import3d FilePath
           | Shape Shape
           -- Combinations
           | Union [Solid]
           | Intersection [Solid]
           | Difference Solid Solid
           | Minkowski [Solid]
           | Hull [Solid]
           -- Transformations
           | Scale Vector Solid
           | Resize Vector Solid
           | Rotate Vector Solid
           | Translate Vector Solid
           | Mirror Vector Solid
           | MultMatrix Transform Solid
           | Color (Colour Float) Solid
           | Transparent (AlphaColour Float) Solid
           | LinearExtrude Float Float Point Int Int Facet Shape
           | RotateExtrude Int Facet Shape
           -- Mesh control
           | Var Facet [Solid]
           deriving Show

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
render (Shape s) = rShape s
render (Union ss) = rList "union()" ss
render (Intersection ss) = rList "intersection()" ss
render (Difference s1 s2) = "difference(){" ++ render s1 ++ render s2 ++ "}\n\n"
render (Minkowski ss) = rList "minkowski()" ss
render (Hull ss) = rList "hull()" ss
render (Scale v s) = rVecSolid "scale" v s
render (Resize v s) = rVecSolid "resize" v s
render (Translate v s) = rVecSolid "translate" v s
render (Rotate v s) = "rotate(a=" ++ rVector v ++ ")" ++ render s
render (Mirror v s) = rVecSolid "mirror" v s
render (MultMatrix (a, b, c, d) s) =
    "multmatrix([" ++ rQuad a ++ "," ++ rQuad b ++ "," ++ rQuad c ++ ","
    ++ rQuad d ++"])\n" ++ render s
render (Color c s) = let r = toSRGB c in
    "color(" ++ rVector (channelRed r, channelGreen r, channelBlue r) ++ ")\n"
    ++ render s
render (Transparent c s) =
    "color(" ++ rQuad (channelRed r, channelGreen r, channelBlue r, a) ++ ")"
    ++ render s
    where r = toSRGB $ toPure c
          a = alphaChannel c
          toPure ac = if a > 0 then darken (recip a) (ac `over` black) else black
render (LinearExtrude h t sc sl c f sh) =
    "linear_extrude(height=" ++ show h ++ ",twist=" ++ show t ++ ",scale="
    ++ rPoint sc ++ ",slices=" ++ show sl ++ ",convexity=" ++ show c ++ rFacet f
    ++ ")" ++ rShape sh
render (RotateExtrude c f sh) =
  "rotate_extrude(convexity=" ++ show c ++ rFacet f ++ ")" ++ rShape sh
render (Var (Fa f) ss) = rList ("assign($fa=" ++ show f ++ ")") ss
render (Var (Fs f) ss) = rList ("assign($fs=" ++ show f ++ ")") ss
render (Var (Fn n) ss) = rList ("assign($fn=" ++ show n ++ ")") ss

-- | A convenience function to render a list of 'Solid's by taking
-- their union.
renderL :: [Solid] -> String
renderL = render . Union

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
  "projection(cut=" ++ (if c then "true)" else "false)") ++ render s
rShape (Scale2d p s) = "scale(" ++ rPoint p ++ ")" ++ rShape s
rShape (Resize2d p s) = "resize(" ++ rPoint p ++ ")" ++ rShape s
rShape (Rotate2d p s) = "rotate(" ++ rPoint p ++ ")" ++ rShape s
rShape (Translate2d p s) = "translate(" ++ rPoint p ++ ")" ++ rShape s
rShape (Mirror2d p s) = "mirror(" ++ rPoint p ++ ")" ++ rShape s

-- And some misc. rendering utilities.
rList n ss = n ++ "{\n" ++  concatMap render ss ++ "}"
rSolid n s = n ++ "()\n" ++ render s
rVector (a, b, c) = "[" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "]"
rVecSolid n v s = n ++ "(" ++ rVector v ++ ")\n" ++ render s
rQuad (w, x, y, z) =
  "[" ++ show w ++ "," ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
rFacet Def = ""
rFacet f = "," ++ showFacet f
rPoint (x, y)  = "[" ++ show x ++ "," ++ show y ++ "]"

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

-- | __UNTESTED__ 'import3d' is @import /filename/@, where /filename/
-- is an stl file.  It's /3d/ because import is a key word.
import3d :: FilePath -> Solid
import3d = Import3d

-- | __UNTESTED__ 'import2d' is @import /filename/@, where /filename/
-- is an image or other 2d object.
import2d :: FilePath -> Shape
import2d = Import2d

-- | Create the union of a list of 'Solid's.
union :: [Solid] -> Solid
union = Union

-- | Create the intersection of a list of 'Solid's.
intersection :: [Solid] -> Solid
intersection = Intersection

-- | The difference between two 'Solid's.
difference :: Solid -> Solid -> Solid
difference = Difference

-- | The Minkowski sum of a list of 'Solid's.
minkowski :: [Solid] -> Solid
minkowski = Minkowski

-- | The convex hull of a list of 'Solid's.
hull :: [Solid] -> Solid
hull = Hull

-- | Scale a 'Solid', specifying the scale factor for each axis.
scale :: Vector -> Solid -> Solid
scale = Scale

-- | __UNTESTED__ Resize a 'Solid' to occupy the given dimensions.
resize :: Vector -> Solid -> Solid
resize = Resize

-- | Rotate a 'Solid' by different amounts around each of the three axis.
rotate :: Vector -> Solid -> Solid
rotate = Rotate

-- | Translate a 'Solid' along a 'Vector'.
translate :: Vector -> Solid -> Solid
translate = Translate

-- | Mirror a 'Solid' across a plane intersecting the origin.
mirror :: Vector -> Solid -> Solid
mirror = Mirror

-- | Transform a 'Solid' with a 'Transform' matrix.
multMatrix :: Transform -> Solid -> Solid
multMatrix = MultMatrix

-- | A 'translate' that just goes up, since those seem to be common.
up :: Float -> Solid -> Solid
up f = Translate (0, 0, f)


-- | Render a 'Solid' in a specific color. This doesn't us the
-- OpenSCAD color model, but instead uses the 'Data.Colour' model. The
-- 'Graphics.OpenSCAD' module rexports 'Data.Colour.Names' so you can
-- conveniently say @'color' 'red' /'Solid'/@.
color :: Colour Float -> Solid -> Solid
color = Color

-- | Render a 'Solid' in a transparent color. This uses the
-- 'Data.Coulor.AphaColour' color model.
transparent :: AlphaColour Float -> Solid -> Solid
transparent = Transparent

-- | Extrude a 'Shape' along a line with @linear_extrude@.
linearExtrude :: Float         -- ^ height
              -> Float         -- ^ twist
              -> Point         -- ^ scale
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

-- | Use 'diam' to turn a diameter into a radius for circles, spheres, etc.
diam :: Float -> Float
diam = (/ 2)

-- | Create a rectangular 'Shape' with @rectangle /x-size y-size/@.
rectangle :: Float -> Float -> Shape
rectangle = Rectangle

-- | Create a rectangular 'Solid' with @rectangle /x-size y-size/@.
rectangle3d :: Float -> Float -> Solid
rectangle3d w d = Shape $ Rectangle w d

-- | 'square' is a 'rectangle' with both sides the same size.
square :: Float -> Shape
square s = rectangle s s

-- | 'square3d' is a 'rectangle3d' with both sides the same size.
square3d :: Float -> Solid
square3d s = rectangle3d s s

-- | Create a circular 'Shape' with @circle /radius/ 'Facet'@.
circle :: Float -> Facet -> Shape
circle = Circle

-- | Create a circular 'Solid' with @circle /radius/ 'Facet'@.
circle3d :: Float -> Facet -> Solid
circle3d r f = Shape $ Circle r f

-- | Project a 'Solid' into a 'Shape' with @projection /cut 'Solid'/@.
projection :: Bool -> Solid -> Shape
projection = Projection

-- | Project a 'Solid' to a thin 'Solid' with @projection /cut 'Solid'/@.
projection3d :: Bool -> Solid -> Solid
projection3d c s = Shape $ Projection c s

-- | 'scale2d' is 'scale' for 'Shape's.
scale2d :: Point -> Shape -> Shape
scale2d = Scale2d

-- | 'resize2d' is 'resize' for 'Shape's.
resize2d :: Point -> Shape -> Shape
resize2d = Resize2d

-- | 'rotate2d' is 'rotate' for 'Shape's.
rotate2d :: Point -> Shape -> Shape
rotate2d = Rotate2d

-- | 'translate2d' is 'translate' for 'Shape's.
translate2d :: Point -> Shape -> Shape
translate2d = Translate2d

-- | 'mirror2d' is 'mirror' for 'Shape's.
mirror2d :: Point -> Shape -> Shape
mirror2d = Mirror2d

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
