 {-# LANGUAGE FlexibleInstances #-}

{- |
Module      : Graphics.OpenSCAD
Description : Type-checked wrappers for the OpenSCAD primitives.
Copyright   : &#xa9; Mike Meyer, 2014
License     : BSD4
Maintainer  : mwm@mired.org
Stability   : experimental

= Overview

The Graphics.OpenSCAD module provides abstract data types for creating
OpenSCAD model definitions calls, along with a function to render it
as a string, and some utilities. The primary goal is that the output
should always be valid OpenSCAD. If you manage to generate OpenSCAD
source that causes OpenSCAD to complain, please open an issue.

The primary effect of this is that Graphics.OpenSCAD distinguishes
between 2d and 3d 'Model's. If you want to mix them, you must
explicitly convert between them.  While two-dimensional model creation
could be polymorphic functions that create either, so that such models
could be treated as either 2d or 3d, you'd still have to explicitly
convert models whose type was fixed as 2d by a transformation, and
'render' wouldn't work if the type was still ambiguous, ala @render $
square 2@.

= Usage

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

The type constructors are not exported, with functions being exported
in their stead.  This allows extra checking to be done on those that
need it.  It also provides consistency, as otherwise you'd have to
remember whether 'box' is a constructor or a convenience function,
etc.

Because of this, the constructors are not documented, the exported
functions are. The documentation is generally just the corresponding
OpenSCAD function name, along with the names of the arguments from the
OpenSCAD documentation. If no OpenSCAD function name is given, then
it's the same as the 'Graphics.OpenSCAD' function. You should check
the OpenSCAD documentation for usage information.

= Oddities

'importFile' has been left polymorphic. I couldn't find a sane way to
check that you're importing the right file type, so detecting such
errors - including importing a 3d file and trying to extrude it - have
to be left up to OpenSCAD in any case.  So for now, there's just
'importFile'. This does create the oddity that if you import a file
and try and render it without doing something to indicate how many
dimensions it has (one of the transformations, an extrusion or
projection, or 'solid') you'll get a compile error because the type is
ambiguous. Later, this may turn into @import2d@ and @import3d@.

The interfaces for 'polygon's and 'polyhedron's is seriously different
from the OpenSCAD interface. Rather than expecting you to enter a list
of points and then references to them, you just enter the points
directly. If you really want to do it the OpenSCAD way, you can do
something like:

@
draw $ polyhedron [[(p 0, p 1, p 2), (p 0, p 2, p 3), ... ]]
where points = [.....]
      p i = points !! i
@

Also, the OpenSCAD polyedron code recently changed. The old version
requires that the faces all be triangles, the new version allows for
them to be arbitrary polygons. 'Graphics.OpenSCAD' supports both: if
all your faces are triangles, it will use the old version. If some
have more points, the new version will be used. If any have fewer than
three points you get an error. At this time, no tests are done on the
faces. That will probably change in the future.

Finally, polygon and polyhedron can generate errors on input that
seems to generate proper solids. If you turn on 'View->Thrown
Together', you'll see it highlighting errors in the object.

Offset is missing even though it's documented, as it isn't supported
by a released version of OpenSCAD, so presumably subject to change. It
is implemented, but untested as yet. You can add it to the module's
export lists if you want to play with it.

-}

module Graphics.OpenSCAD (
  -- * Types
  -- ** A 'Model' to be rendered, and a 'Vector' that fixes the
  -- number of dimensions it has.
  Model, Vector,
  -- ** Types aliases with fixed dimensions
  Model2d, Model3d, Vector2d, Vector3d,
  --  ** Other type aliases
  Facet, TransMatrix,
  -- ** Type for 'unsafePolyhedron' 'Sides' argument
  Sides(..),
  -- * Primitive creation
  -- ** 'Model2d's
  rectangle, square, circle, polygon, unsafePolygon, projection, importFile,
  -- ** 'Model3d's
  sphere, box, cube, cylinder, obCylinder, polyhedron, unsafePolyhedron,
  multMatrix, linearExtrude, rotateExtrude, surface, solid,
  -- * Functions
  -- ** Combinations
  union, intersection, difference, minkowski, hull,
  -- ** Transformations
  scale, resize, rotate, translate, mirror, color, transparent, up,
  -- ** Rendering
  render, renderL,
  -- ** 'Facet's.
  var, fn, fs, fa, def,
  -- ** General convenience functions
  diam, draw, drawL, (#),
  module Colours)

where

import Data.Colour (Colour, AlphaColour, alphaChannel, darken, over, black)
import Data.Colour.Names as Colours
import Data.Colour.SRGB (channelRed, channelBlue, channelGreen, toSRGB)
import Data.List (elemIndices, nub, intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Monoid ((<>), Monoid, mconcat, mempty, mappend)
import qualified Data.Set as Set
import System.FilePath (FilePath)

-- A vector in 2 or 3-space. They are used in transformations of
-- 'Model's of their type.
class Eq a => Vector a where
  rVector :: a -> String
  toList :: a -> [Double]
  (#*) :: a -> a -> a      -- cross product
  (#-) :: a -> a -> a      -- difference between two vectors

  (#.) :: a -> a -> Double -- dot product
  v1 #. v2 = sum $ zipWith (*) (toList v1) (toList v2)

  isZero :: a -> Bool      -- is a zero vector. Arguably should use eps.
  isZero = all (== 0) . toList

  collinear :: [a] -> Bool -- are all points collinear?
  collinear [] = False
  collinear [_] = False
  collinear [v1, v2] = v1 /= v2
  collinear (v1:v2:vs)
    | v1 /= v2  = all (\v -> isZero $ (v2 #- v1) #* (v1 #- v)) vs
    | otherwise = collinear (v2:vs)

-- | 'Vector2d' is used where 'Graphics.OpenSCAD' expects an OpenSCAD
-- @vector@ of length 2.
type Vector2d = (Double, Double)
instance Vector Vector2d where
  rVector (x, y) = "[" ++ show x ++ "," ++ show y ++ "]"
  toList (x, y) = [x, y]
  (x1, y1) #- (x2, y2) = (x1 - x2, y1 - y2)
  (x1, y1) #* (x2, y2) = (0, x1 * y2 - y1 * x2) -- for purposes of collinear

-- | 'Vector3d' is used where 'Graphics.OpenSCAD' expects an OpenSCAD
-- @vector@ of length 3.
type Vector3d = (Double, Double, Double)
instance Vector Vector3d where
  rVector (x, y, z) = "[" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]"
  toList (x, y, z) = [x, y, z]
  (x1, y1, z1) #- (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)
  (x1, y1, z1) #* (x2, y2, z2) = (y1 * z2 - z1 * y2,
                                  z1 * x2 - x1 * z2,
                                  x1 * y2 - y1 * x2)

-- Coplanar only makes sense for R3, so it's not part of the Vector class
coplanar :: [Vector3d] -> Bool
coplanar vs | length vs <= 3        = True -- by definition
            | collinear $ take 3 vs = coplanar $ tail vs
            | otherwise =
                all (\v -> (v3 #- v1) #. ((v2 #- v1) #* (v #- v3)) == 0) vs'
                where (v1:v2:v3:vs') = vs


-- | A 4x4 transformation matrix specifying a complete 3-space
-- transform of a 'Model3d'.
type TransMatrix =
  ((Double, Double, Double, Double), (Double, Double, Double, Double),
   (Double, Double, Double, Double), (Double, Double, Double, Double))


-- While it's tempting to add more options to Solid, Shape or Model,
-- don't do it. Instead, add functions that add that functionality,
-- by building the appropriate structure, like cube vs. box.

-- | A 'Facet' is used to set one of the special variables that
-- control the mesh used during generation of circular objects. They
-- appear as arguments to various constructors, as well as in the
-- 'var' function to set them for the argument objects.
data Facet = Fa Double | Fs Double | Fn Int | Def deriving Show

-- | A 'Join' controls how edges in a 'polygon' are joined by the
-- 'offset' operation.
data Join = Bevel | Round | Miter Double deriving Show

-- A 'Shape' is a 2-dimensional primitive to be used in a 'Model2d'.
data Shape = Rectangle Double Double
           | Circle Double Facet
           | Polygon Int [Vector2d] [[Int]]
           | Projection Bool Model3d
           | Offset Double Join Shape
           deriving Show

-- | The third argument to unsafePolyhedron is a 'Sides'.
data Sides = Faces [[Int]] | Triangles [[Int]] deriving Show

-- A 'Solid' is a 3-dimensional primitive to be used in a 'Model3d'.
data Solid = Sphere Double Facet
           | Box Double Double Double
           | Cylinder Double Double Facet
           | ObCylinder Double Double Double Facet
           | Polyhedron Int [Vector3d] Sides
           | MultMatrix TransMatrix Model3d
           | LinearExtrude Double Double Vector2d Int Int Facet Model2d
           | RotateExtrude Int Facet Model2d
           | Surface FilePath Bool Int
           | ToSolid Model2d
           deriving Show

-- | A 'Model' is either a 'Model2d', a 'Model3d', a transformation of
-- a 'Model', a combination of 'Model's, or a 'Model' with it's
-- rendering tweaked by a 'Facet'. 'Model's can be rendered.
data Model v = Shape Shape
             | Solid Solid
             | Scale v (Model v)
             | Resize v (Model v)
             | Rotate v (Model v)
             | Translate v (Model v)
             | Mirror v (Model v)
             | Color (Colour Double) (Model v)
             | Transparent (AlphaColour Double) (Model v)
             -- and combinations
             | Union [Model v]
             | Intersection [Model v]
             | Minkowski [Model v]
             | Hull [Model v]
             | Difference (Model v) (Model v)
             -- And oddball stuff control
             | Import FilePath
             | Var Facet [Model v]
             deriving Show

-- | A two-dimensional model. Note that the types do not mix
-- implicitly. You must turn a 'Model2d' into a 'Model3d' using one of
-- 'linearExtrude', 'rotateExtrude', or 'solid'.
type Model2d = Model Vector2d

-- | A three-dimensional model. You can create a 'Model2d' from a
-- 'Model3d' using 'projection'.
type Model3d = Model Vector3d

-- Tools for creating 'Model2d's.
-- | Create a rectangular 'Model2d' with @rectangle /x-size y-size/@.
rectangle :: Double -> Double -> Model2d
rectangle w h = Shape $ Rectangle w h

-- | 'square' is a 'rectangle' with both sides the same size.
square :: Double -> Model2d
square s = rectangle s s

-- | Create a circular 'Model' with @circle /radius/ 'Facet'@.
circle :: Double -> Facet -> Model2d
circle r f = Shape $ Circle r f

-- | Project a 'Model3d' into a 'Model' with @projection /cut 'Model3d'/@.
projection :: Bool -> Model3d -> Model2d
projection c s = Shape $ Projection c s

-- | Turn a list of lists of 'Vector2d's and an Int into @polygon
-- /convexity points path/@. The argument to polygon is the list of
-- paths that is the second argument to the OpenSCAD polygon function,
-- except the points are 'Vector2d's, not references to 'Vector2d's in
-- that functions points argument.  If you were just going to pass in
-- the points, it now needs to be in an extra level of 'List'.
polygon ::  Int -> [[Vector2d]] -> Model2d
polygon convexity paths
  | any ((< 3) . length) paths = error "Polygon has fewer than 3 points."
  | any collinear paths = error "Points in polygon are collinear."
  | otherwise = let points = nub $ concat paths
                in Shape . Polygon convexity points
                   $ map (concatMap (`elemIndices` points)) paths

-- | This provides direct access to the OpenScad @polygon@ command for
-- performance reasons. This version uses the OpenSCAD arguments:
-- @polygon /convexity points path/@ to allow client code to save
-- space.  However, it bypasses all the checks done by
-- 'polygon', which need the other representation.
unsafePolygon :: Int -> [Vector2d] -> [[Int]] -> Model2d
unsafePolygon convexity points paths = Shape $ Polygon convexity points paths

-- | 'offset' a 'Model2d's edges by @offset /delta join/@.
offset :: Double -> Join -> Model2d -> Model2d
offset d j (Shape s) = Shape $ Offset d j s

-- Tools for creating Model3ds
-- | Create a sphere with @sphere /radius 'Facet'/@.
sphere :: Double -> Facet -> Model3d
sphere r f = Solid $ Sphere r f

-- | Create a box with @cube /x-size y-size z-size/@
box :: Double -> Double -> Double -> Model3d
box x y z = Solid $ Box x y z

-- | A convenience function for creating a cube as a 'box' with all
-- sides the same length.
cube :: Double -> Model3d
cube x = box x x x

-- | Create a cylinder with @cylinder /radius height 'Facet'/@.
cylinder :: Double -> Double -> Facet -> Model3d
cylinder h r f = Solid $ Cylinder h r f

-- | Create an oblique cylinder with @cylinder /radius1 height radius2 'Facet'/@.
obCylinder :: Double -> Double -> Double -> Facet -> Model Vector3d
obCylinder r1 h r2 f= Solid $ ObCylinder r1 h r2 f

-- | Turn a list of list of 'Vector3d's and an int into @polyhedron
-- /convexity points 'Sides'/@. The argument to polyhedron is the list
-- of paths that is the second argument to the OpenSCAD polyhedron
-- function, except the points are 'Vector3d's, not the references to
-- 'Vector3d's used in that functions @points@ argument.  The function
-- will build the appropriate function call, using @faces@ if you pass
-- in a side that uses more than 3 points, or @triangles@ if not. Note
-- that @faces@ doesn't work in older versions of OpenSCAD, and
-- @triangles@ is depreciated. Until a mechanism to set the version of
-- OpenSCAD is provided, generating the @faces@ version will cause an
-- error.
--
-- Passing in 'Sides' that have fewer than three points, have
-- collinear points or have points that aren't in the same plane is an
-- error that is caught by the library.
polyhedron ::  Int -> [[Vector3d]] -> Model3d
polyhedron convexity paths
  | any ((< 3) . length) paths = error "Some face has fewer than 3 points."
  | any collinear paths = error "Some face has collinear points."
  | any (not . coplanar) paths = error "Some face isn't coplanar."
  | length vectors /= length (nub vectors) =
    error "Some faces have different orientation."
  | 2 * length edges /= length vectors = error "Some edges are not in two faces."
  | xCross headMax xMax tailMax > 0 =
    error "Face orientations are counterclockwise."
  | otherwise = Solid . Polyhedron convexity points $ sides sidesIn
  where vectors = concatMap (\p -> zip p (tail p ++ [head p])) paths
        edges = nub $ map (Set.fromList . \(a, b) -> [a, b]) vectors
        points = nub $ concat paths
        xMax = maximum points
        faceMax = head $ filter (elem xMax) paths
        (maxFirst, maxLast) = break (== xMax) faceMax
        (headMax, tailMax)  = (if null maxFirst
                                  then last maxLast
                                  else last maxFirst,
                               if null (tail maxLast)
                                  then head maxFirst
                                  else head (tail maxLast))
        xCross a b c  = (\(a, b, c) -> a) $ (a #- b) #* (b #- c)
        sidesIn = map (concatMap (`elemIndices` points)) paths
        sides ss | any ((> 3) . length) ss  = Faces ss
                 | all ((== 3) . length) ss = Triangles ss
                 | otherwise = error "Some faces have fewer than 3 points."

-- | This provides direct access to the OpenSCAD @polyhedron@ command
-- for performance reasons.  This version uses the OpenSCAD arguments:
-- @polyhedron /convexity points 'Sides'/@ to allow client code to
-- save space. However, it bypasses all the checks done by
-- 'polyhedron', which needs the other representation.
unsafePolyhedron :: Int -> [Vector3d] -> Sides -> Model3d
unsafePolyhedron convexity points sides = Solid $ Polyhedron convexity points sides


-- | Transform a 'Model3d' with a 'TransMatrix'
multMatrix :: TransMatrix -> Model3d -> Model3d
multMatrix t m = Solid $ MultMatrix t m

-- | Turn a 'Model2d' into a 'Model3d' exactly as is.
solid :: Model2d -> Model3d
solid = Solid . ToSolid

-- | Extrude a 'Model2d' along a line with @linear_extrude@.
linearExtrude :: Double   -- ^ height
              -> Double   -- ^ twist
              -> Vector2d -- ^ scale
              -> Int      -- ^ slices
              -> Int      -- ^ convexity
              -> Facet
              -> Model2d  -- ^ to extrude
              -> Model3d
linearExtrude h t sc sl c f m = Solid $ LinearExtrude h t sc sl c f m

-- | Rotate a 'Model2d' around the origin with @rotate_extrude
-- /convexity 'Facet' 'Model'/@
rotateExtrude ::  Int -> Facet -> Model2d -> Model3d
rotateExtrude c f m = Solid $ RotateExtrude c f m

-- | Load a height map from a file with @surface /FilePath Invert Convexity/@.
surface :: FilePath -> Bool -> Int -> Model3d
surface f i c = Solid $ Surface f i c

-- And the one polymorphic function we have.
-- | 'importFile' is @import /filename/@.
importFile :: Vector v => FilePath -> Model v
importFile = Import


-- Transformations
-- | Scale a 'Model', the vector specifying the scale factor for each axis.
scale :: Vector v => v -> Model v -> Model v
scale = Scale

-- | Resize a 'Model' to occupy the dimensions given by the vector. Note that
-- this does nothing prior to the 2014 versions of OpenSCAD.
resize :: Vector v => v -> Model v -> Model v
resize = Resize

-- | Rotate a 'Model' by different amounts around each of the three axis.
rotate :: Vector v => v -> Model v -> Model v
rotate = Rotate

-- | Translate a 'Model' along a 'Vector'.
translate :: Vector v => v -> Model v -> Model v
translate = Translate

-- | Mirror a 'Model' across a plane intersecting the origin.
mirror :: Vector v => v -> Model v -> Model v
mirror = Mirror

-- | Render a 'Model' in a specific color. This doesn't use the
-- OpenSCAD color model, but instead uses the 'Data.Colour' model. The
-- 'Graphics.OpenSCAD' module rexports 'Data.Colour.Names' so you can
-- conveniently say @'color' 'red' /'Model'/@.
color :: Vector v => Colour Double -> Model v -> Model v
color = Color

-- | Render a 'Model' in a transparent color. This uses the
-- 'Data.Colour.AlphaColour' color model.
transparent :: Vector v => AlphaColour Double -> Model v -> Model v
transparent = Transparent

-- | A 'translate' that just goes up, since those seem to be common.
up :: Double -> Model3d -> Model3d
up f = translate (0, 0, f)


-- Combinations
-- | Create the union of a list of 'Model's.
union :: Vector v => [Model v] -> Model v
union = Union

-- | Create the intersection of a list of 'Model's.
intersection :: Vector v => [Model v] -> Model v
intersection = Intersection

-- | The difference between two 'Model's.
difference :: Vector v => Model v -> Model v -> Model v
difference = Difference

-- | The Minkowski sum of a list of 'Model's.
minkowski :: Vector v => [Model v] -> Model v
minkowski = Minkowski

-- | The convex hull of a list of 'Model's.
hull :: Vector v => [Model v] -> Model v
hull = Hull


-- | 'render' does all the real work. It will walk the AST for a 'Model',
-- returning an OpenSCAD program in a 'String'.
render :: Vector v => Model v -> String
render (Shape s) = rShape s
render (Solid s) = rSolid s
render (Union ss) = rList "union()" ss
render (Intersection ss) = rList "intersection()" ss
render (Difference s1 s2) = "difference(){" ++ render s1 ++ render s2 ++ "}\n"
render (Minkowski ss) = rList "minkowski()" ss
render (Hull ss) = rList "hull()" ss
render (Scale v s) = rVecSolid "scale" v s
render (Resize v s) = rVecSolid "resize" v s
render (Translate v s) = rVecSolid "translate" v s
render (Rotate v s) = "rotate(" ++ rVector v ++ ")" ++ render s
render (Mirror v s) = rVecSolid "mirror" v s
render (Import f) = "import(\"" ++ f ++ "\");\n"
render (Color c s) = let r = toSRGB c in
    "color(" ++ rVector (channelRed r, channelGreen r, channelBlue r) ++ ")\n"
    ++ render s
render (Transparent c s) =
    "color(" ++ rQuad (channelRed r, channelGreen r, channelBlue r, a) ++ ")"
    ++ render s
    where r = toSRGB $ toPure c
          a = alphaChannel c
          toPure ac = if a > 0 then darken (recip a) (ac `over` black) else black
render (Var (Fa f) ss) = rList ("assign($fa=" ++ show f ++ ")") ss
render (Var (Fs f) ss) = rList ("assign($fs=" ++ show f ++ ")") ss
render (Var (Fn n) ss) = rList ("assign($fn=" ++ show n ++ ")") ss

-- utility for rendering Shapes.
rShape :: Shape -> String
rShape (Rectangle r f) = "square([" ++ show r ++ "," ++ show f ++ "]);\n"
rShape (Circle r f) = "circle(" ++ show r ++ rFacet f ++ ");\n"
rShape (Projection c s) =
  "projection(cut=" ++ (if c then "true)" else "false)") ++ render s
rShape (Polygon c points paths) = "polygon(points=" ++ rVectorL points ++
 ",paths=" ++ show paths ++ ",convexity=" ++ show c ++ ");\n"
rShape (Offset d j s) =
  "offset(delta=" ++ show d ++ "," ++ rJoin j ++ ")" ++ rShape s

-- utility for rendering Joins
rJoin :: Join -> String
rJoin Bevel = "join_type=bevel"
rJoin Round = "join_type=round"
rJoin (Miter l) = "miter_limit=" ++ show l

-- utilities for rendering Solids.
rSolid :: Solid -> String
rSolid (Sphere x f) = "sphere(" ++ show x ++ rFacet f ++ ");\n"
rSolid (Box x y z) =
  "cube([" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "]);\n"
rSolid (Cylinder r h f) =
  "cylinder(r=" ++ show r ++ ",h=" ++ show h ++ rFacet f ++ ");\n"
rSolid (ObCylinder r1 h r2 f) =
    "cylinder(r1=" ++ show r1 ++ ",h=" ++ show h ++ ",r2=" ++ show r2 ++ rFacet f
    ++ ");\n"
rSolid (Polyhedron c ps ss) = "polyhedron(points=" ++  rVectorL ps ++ rSides ss
                              ++ ",convexity=" ++ show c ++ ");\n"
rSolid (MultMatrix (a, b, c, d) s) =
    "multmatrix([" ++ rQuad a ++ "," ++ rQuad b ++ "," ++ rQuad c ++ ","
    ++ rQuad d ++"])\n" ++ render s
rSolid (LinearExtrude h t sc sl c f sh) =
    "linear_extrude(height=" ++ show h ++ ",twist=" ++ show t ++ ",scale="
    ++ rVector sc ++ ",slices=" ++ show sl ++ ",convexity=" ++ show c
    ++ rFacet f ++ ")" ++ render sh
rSolid (RotateExtrude c f sh) =
  "rotate_extrude(convexity=" ++ show c ++ rFacet f ++ ")" ++ render sh
rSolid (Surface f i c) =
  "surface(file=\"" ++ f ++ "\"," ++ (if i then "invert=true," else "")
  ++ "convexity=" ++ show c ++ ");\n"
rSolid (ToSolid s) = render s

-- render a list of vectors as an Openscad vector of vectors.
rVectorL vs = "[" ++ intercalate "," (map rVector vs) ++ "]"

-- render a Sides.
rSides (Faces vs) = ",faces=" ++ rListL vs
rSides (Triangles vs) = ",triangles=" ++ rListL vs
rListL vs = "[" ++ intercalate "," (map show vs) ++ "]"

-- | A convenience function to render a list of 'Model's by taking
-- their union.
renderL :: Vector v => [Model v] -> String
renderL = render . union

-- | A convenience function to write the rendered 'Model' to
-- standard output.
draw :: Vector v => Model v -> IO ()
draw = putStrLn . render

-- | A convenience function to write a 'union' of 'Model's to
-- standard output.
drawL :: Vector v => [Model v] -> IO ()
drawL = draw . Union

-- And some misc. rendering utilities.
rList n ss = n ++ "{\n" ++  concatMap render ss ++ "}"
rVecSolid n v s = n ++ "(" ++ rVector v ++ ")\n" ++ render s
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

-- Convenience functions for Facets.
-- | 'var' uses @assign@ to set a 'Facet' variable for it's 'Model's.
var :: Facet -> [Model v] -> Model v
var = Var

-- | 'fa' is used to set the @$fa@ variable in a 'Facet' or 'var'.
fa :: Double -> Facet
fa = Fa

-- | 'fs' is used to set the @$fs@ variable in a 'Facet' or 'var'.
fs :: Double -> Facet
fs = Fs

-- | 'fn' is used to set the @$fn@ variable in a 'Facet' or 'var'.
fn :: Int -> Facet
fn = Fn

-- | 'def' is used where a 'Facet' is needed but we don't want to change
-- any of the values.
def :: Facet
def = Def

-- And one last convenience function.
-- | Use 'diam' to turn a diameter into a radius for circles, spheres, etc.
diam :: Double -> Double
diam = (/ 2)

-- Now, let Haskell work it's magic
instance Vector v => Monoid (Model v) where
  mempty = Solid $ Box 0 0 0
  mappend (Solid (Box 0 0 0)) b = b
  mappend a (Solid (Box 0 0 0)) = a
  mappend a b = union [a, b]
  mconcat [a] = a
  mconcat as = union as


-- | You can use '(#)' to write transformations in a more readable postfix form,
--   cube 3 # color red # translate (-3, -3, -3)
infixl 8 #
(#) = flip ($)
