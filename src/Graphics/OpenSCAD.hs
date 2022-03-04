{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      : Graphics.OpenSCAD
-- Description : Type-checked wrappers for the OpenSCAD primitives.
-- Copyright   : &#xa9; Mike Meyer, 2014
-- License     : BSD4
-- Maintainer  : mwm@mired.org
-- Stability   : experimental
--
-- = Overview
--
-- The Graphics.OpenSCAD module provides abstract data types for creating
-- OpenSCAD model definitions calls, along with a function to render it
-- as a string, and some utilities. The primary goal is that the output
-- should always be valid OpenSCAD. If you manage to generate OpenSCAD
-- source that causes OpenSCAD to complain, please open an issue.
--
-- The primary effect of this is that Graphics.OpenSCAD distinguishes
-- between 2d and 3d 'Model's. If you want to mix them, you must
-- explicitly convert between them.  While two-dimensional model creation
-- could be polymorphic functions that create either, so that such models
-- could be treated as either 2d or 3d, you'd still have to explicitly
-- convert models whose type was fixed as 2d by a transformation, and
-- 'render' wouldn't work if the type was still ambiguous, ala @render $
-- square 2@.
--
-- = Usage
--
-- Standard usage is to have a @main@ function that looks like:
--
-- @
-- main = draw $ /Solid/
-- @
-- or
-- @
-- main = drawL $ [/Solid/]
-- @
--
-- and then set your IDE's compile command to use @runhaskell@ or
-- equivalent to run your code and send the output to a .scad file. Open
-- that file in OpenSCAD, and set it to automatically reload if the file
-- changes. Recompiling your program will cause the model to be loaded
-- and displayed by OpenSCAD.
--
-- The type constructors are not exported, with functions being exported
-- in their stead.  This allows extra checking to be done on those that
-- need it.  It also provides consistency, as otherwise you'd have to
-- remember whether 'box' is a constructor or a convenience function,
-- etc.
--
-- Because of this, the constructors are not documented, the exported
-- functions are. The documentation is generally just the corresponding
-- OpenSCAD function name, along with the names of the arguments from the
-- OpenSCAD documentation. If no OpenSCAD function name is given, then
-- it's the same as the 'Graphics.OpenSCAD' function. You should check
-- the OpenSCAD documentation for usage information.
--
-- = Oddities
--
-- 'importFile' has been left polymorphic. I couldn't find a sane way to
-- check that you're importing the right file type, so detecting such
-- errors - including importing a 3d file and trying to extrude it - have
-- to be left up to OpenSCAD in any case.  So for now, there's just
-- 'importFile'. This does create the oddity that if you import a file
-- and try and render it without doing something to indicate how many
-- dimensions it has (one of the transformations, an extrusion or
-- projection, or 'solid') you'll get a compile error because the type is
-- ambiguous. Later, this may turn into @import2d@ and @import3d@.
--
-- The interfaces for 'polygon's and 'polyhedron's is seriously different
-- from the OpenSCAD interface. Rather than expecting you to enter a list
-- of points and then references to them, you just enter the points
-- directly. If you really want to do it the OpenSCAD way, you can do
-- something like:
--
-- @
-- draw $ polyhedron [[(p 0, p 1, p 2), (p 0, p 2, p 3), ... ]]
-- where points = [.....]
--       p i = points !! i
-- @
--
-- Also, the OpenSCAD polyedron code recently changed. The old version
-- requires that the faces all be triangles, the new version allows for
-- them to be arbitrary polygons. 'Graphics.OpenSCAD' supports both: if
-- all your faces are triangles, it will use the old version. If some
-- have more points, the new version will be used. If any have fewer than
-- three points you get an error. At this time, no tests are done on the
-- faces. That will probably change in the future.
--
-- Finally, polygon and polyhedron can generate errors on input that
-- seems to generate proper solids. If you turn on 'View->Thrown
-- Together', you'll see it highlighting errors in the object.
--
-- Offset is missing even though it's documented, as it isn't supported
-- by a released version of OpenSCAD, so presumably subject to change. It
-- is implemented, but untested as yet. You can add it to the module's
-- export lists if you want to play with it.
module Graphics.OpenSCAD
  ( -- * Types

    -- ** A 'Model' to be rendered, and a 'Vector' that fixes the

    -- number of dimensions it has.
    Model,
    Vector,

    -- ** Types aliases with fixed dimensions
    Model2d,
    Model3d,
    Vector2d,
    Vector3d,
    --  ** Other type aliases
    Facets (..),
    TransMatrix,

    -- ** Type for 'unsafePolyhedron' 'Sides' argument
    Sides (..),

    -- * Primitive creation

    -- ** 'Model2d's
    rectangle,
    square,
    circle,
    polygon,
    unsafePolygon,
    projection,
    offset,
    importFile,

    -- ** 'Model3d's
    sphere,
    box,
    cube,
    cylinder,
    obCylinder,
    polyhedron,
    unsafePolyhedron,
    multMatrix,
    linearExtrude,
    rotateExtrude,
    surface,

    -- * Functions

    -- ** Combinations
    union,
    intersection,
    difference,
    minkowski,
    hull,

    -- ** Transformations
    scale,
    resize,
    rotate2d,
    rotate3d,
    translate,
    mirror,
    color,
    transparent,
    up,

    -- ** Rendering
    render,
    renderL,

    -- ** 'Facet's.
    var,
    fn,
    fs,
    fa,
    def,

    -- ** General convenience functions
    diam,
    draw,
    drawL,
    (#),
    module Colours,
  )
where

import Data.Colour (AlphaColour, Colour, alphaChannel, darken, over)
import Data.Colour.Names as Colours
import Data.Colour.SRGB (channelBlue, channelGreen, channelRed, toSRGB)
import Data.List (elemIndices, nub)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Prettyprinter ((<+>))
import qualified Prettyprinter as PP

-- A vector in 2 or 3-space. They are used in transformations of
-- 'Model's of their type.
class Eq a => Vector a where
  rVector :: a -> String
  toList :: a -> [Double]
  (#*) :: a -> a -> a -- cross product
  (#-) :: a -> a -> a -- difference between two vectors

  (#.) :: a -> a -> Double -- dot product
  v1 #. v2 = sum $ zipWith (*) (toList v1) (toList v2)

  isZero :: a -> Bool -- is a zero vector. Arguably should use eps.
  isZero = all (== 0) . toList

  collinear :: [a] -> Bool -- are all points collinear?
  collinear [] = False
  collinear [_] = False
  collinear [v1, v2] = v1 /= v2
  collinear (v1 : v2 : vs)
    | v1 /= v2 = all (\v -> isZero $ (v2 #- v1) #* (v1 #- v)) vs
    | otherwise = collinear (v2 : vs)

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
  (x1, y1, z1) #* (x2, y2, z2) =
    ( y1 * z2 - z1 * y2,
      z1 * x2 - x1 * z2,
      x1 * y2 - y1 * x2
    )

-- Coplanar only makes sense for R3, so it's not part of the Vector class
coplanar :: [Vector3d] -> Bool
coplanar vs
  | length vs <= 3 = True -- by definition
  | collinear $ take 3 vs = coplanar $ tail vs
  | otherwise =
    all (\v -> (v3 #- v1) #. ((v2 #- v1) #* (v #- v3)) == 0) vs'
  where
    (v1 : v2 : v3 : vs') = vs

-- | A 4x4 transformation matrix specifying a complete 3-space
-- transform of a 'Model3d'.
type TransMatrix =
  ( (Double, Double, Double, Double),
    (Double, Double, Double, Double),
    (Double, Double, Double, Double),
    (Double, Double, Double, Double)
  )

-- While it's tempting to add more options to Solid, Shape or Model,
-- don't do it. Instead, add functions that add that functionality,
-- by building the appropriate structure, like cube vs. box.

-- | A 'Facet' is used to set one of the special variables that
-- control the mesh used during generation of circular objects. They
-- appear as arguments to various constructors, as well as in the
-- 'var' function to set them for the argument objects.
data Facets = Facets
  { facetFa :: Maybe Double,
    facetFs :: Maybe Double,
    facetFn :: Maybe Int
  }
  deriving (Show)

-- | A 'Join' controls how edges in a 'polygon' are joined by the
-- 'offset' operation.
data Join = Bevel | Round | Miter Double deriving (Show)

-- | A 'Shape' is a 2-dimensional primitive to be used in a 'Model2d'.
data Shape
  = Rectangle Double Double
  | Circle Double Facets
  | Polygon Int [Vector2d] [[Int]]
  deriving (Show)

-- | The third argument to unsafePolyhedron is a 'Sides'.
data Sides = Faces [[Int]] | Triangles [[Int]] deriving (Show)

-- | A 'Solid' is a 3-dimensional primitive to be used in a 'Model3d'.
data Solid
  = Sphere Double Facets
  | Box Double Double Double
  | Cylinder Double Double Facets
  | ObCylinder Double Double Double Facets
  | Polyhedron Int [Vector3d] Sides
  | Surface FilePath Bool Int
  deriving (Show)

-- | A 'Transformation' is an OpenSCAD operator that changes an existing model
-- into another model
data UnaryOp v where
  Scale :: v -> UnaryOp v
  Resize :: v -> UnaryOp v
  Rotate2d :: Double -> UnaryOp Vector2d
  Rotate3d :: Vector3d -> UnaryOp Vector3d
  Translate :: v -> UnaryOp v
  Mirror :: v -> UnaryOp v
  Color :: Colour Double -> UnaryOp v
  Transparent :: AlphaColour Double -> UnaryOp v
  Offset :: Double -> Join -> UnaryOp Vector2d
  MultMatrix :: TransMatrix -> UnaryOp Vector3d

deriving instance Show (UnaryOp Vector2d)

deriving instance Show (UnaryOp Vector3d)

data BinaryOp v where
  Difference :: BinaryOp v

deriving instance Show (BinaryOp Vector2d)

deriving instance Show (BinaryOp Vector3d)

data ListOp v where
  Union :: ListOp v
  Intersection :: ListOp v
  Minkowski :: ListOp v
  Hull :: ListOp v

deriving instance Show (ListOp Vector2d)

deriving instance Show (ListOp Vector3d)

-- | A 'Model' is either a 'Model2d', a 'Model3d', a transformation of
-- a 'Model', a combination of 'Model's, or a 'Model' with it's
-- rendering tweaked by a 'Facet'. 'Model's can be rendered.
data Model v where
  Shape :: Shape -> Model2d
  Solid :: Solid -> Model3d
  UnaryOp :: UnaryOp v -> Model v -> Model v
  BinaryOp :: BinaryOp v -> Model v -> Model v -> Model v
  ListOp :: ListOp v -> [Model v] -> Model v
  Projection :: Bool -> Model3d -> Model2d
  LinearExtrude :: Double -> Double -> Vector2d -> Int -> Int -> Facets -> Model2d -> Model3d
  RotateExtrude :: Int -> Facets -> Model2d -> Model3d
  -- And oddball stuff control
  Import :: FilePath -> Model v
  Var :: Facets -> [Model v] -> Model v

deriving instance Show Model2d

deriving instance Show Model3d

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
circle :: Double -> Facets -> Model2d
circle r facets = Shape $ Circle r facets

-- | Project a 'Model3d' into a 'Model' with @projection /cut 'Model3d'/@.
projection :: Bool -> Model3d -> Model2d
projection = Projection

-- | Turn a list of lists of 'Vector2d's and an Int into @polygon
-- /convexity points path/@. The argument to polygon is the list of
-- paths that is the second argument to the OpenSCAD polygon function,
-- except the points are 'Vector2d's, not references to 'Vector2d's in
-- that functions points argument.  If you were just going to pass in
-- the points, it now needs to be in an extra level of 'List'.
polygon :: Int -> [[Vector2d]] -> Model2d
polygon convexity paths
  | any ((< 3) . length) paths = error "Polygon has fewer than 3 points."
  | any collinear paths = error "Points in polygon are collinear."
  | otherwise =
    let points = nub $ concat paths
     in Shape . Polygon convexity points $
          map (concatMap (`elemIndices` points)) paths

-- | This provides direct access to the OpenScad @polygon@ command for
-- performance reasons. This version uses the OpenSCAD arguments:
-- @polygon /convexity points path/@ to allow client code to save
-- space.  However, it bypasses all the checks done by
-- 'polygon', which need the other representation.
unsafePolygon :: Int -> [Vector2d] -> [[Int]] -> Model2d
unsafePolygon convexity points paths = Shape $ Polygon convexity points paths

-- | 'offset' a 'Model2d's edges by @offset /delta join/@.
offset :: Double -> Join -> Model2d -> Model2d
offset d j = UnaryOp (Offset d j)

-- Tools for creating Model3ds

-- | Create a sphere with @sphere /radius 'Facet'/@.
sphere :: Double -> Facets -> Model3d
sphere r f = Solid $ Sphere r f

-- | Create a box with @cube /x-size y-size z-size/@
box :: Double -> Double -> Double -> Model3d
box x y z = Solid $ Box x y z

-- | A convenience function for creating a cube as a 'box' with all
-- sides the same length.
cube :: Double -> Model3d
cube x = box x x x

-- | Create a cylinder with @cylinder /radius height 'Facet'/@.
cylinder :: Double -> Double -> Facets -> Model3d
cylinder h r f = Solid $ Cylinder h r f

-- | Create an oblique cylinder with @cylinder /radius1 height radius2 'Facet'/@.
obCylinder :: Double -> Double -> Double -> Facets -> Model Vector3d
obCylinder r1 h r2 f = Solid $ ObCylinder r1 h r2 f

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
polyhedron :: Int -> [[Vector3d]] -> Model3d
polyhedron convexity paths
  | any ((< 3) . length) paths = error "Some face has fewer than 3 points."
  | any collinear paths = error "Some face has collinear points."
  | (not . all coplanar) paths = error "Some face isn't coplanar."
  | length vectors /= length (nub vectors) =
    error "Some faces have different orientation."
  | 2 * length edges /= length vectors = error "Some edges are not in two faces."
  | xCross headMax xMax tailMax > 0 =
    error "Face orientations are counterclockwise."
  | otherwise = Solid . Polyhedron convexity points $ sides sidesIn
  where
    vectors = concatMap (\p -> zip p (tail p ++ [head p])) paths
    edges = nub $ map (Set.fromList . \(a, b) -> [a, b]) vectors
    points = nub $ concat paths
    xMax = maximum points
    faceMax = head $ filter (elem xMax) paths
    (maxFirst, maxLast) = break (== xMax) faceMax
    (headMax, tailMax) =
      ( if null maxFirst
          then last maxLast
          else last maxFirst,
        if null (tail maxLast)
          then head maxFirst
          else head (tail maxLast)
      )
    xCross a b c = (\(a', _b', _c') -> a') $ (a #- b) #* (b #- c)
    sidesIn = map (concatMap (`elemIndices` points)) paths
    sides ss
      | any ((> 3) . length) ss = Faces ss
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
multMatrix m = UnaryOp (MultMatrix m)

-- | Extrude a 'Model2d' along a line with @linear_extrude@.
linearExtrude ::
  -- | height
  Double ->
  -- | twist
  Double ->
  -- | scale
  Vector2d ->
  -- | slices
  Int ->
  -- | convexity
  Int ->
  Facets ->
  -- | to extrude
  Model2d ->
  Model3d
linearExtrude = LinearExtrude

-- | Rotate a 'Model2d' around the origin with @rotate_extrude
-- /convexity 'Facet' 'Model'/@
rotateExtrude :: Int -> Facets -> Model2d -> Model3d
rotateExtrude = RotateExtrude

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
scale v = UnaryOp (Scale v)

-- | Resize a 'Model' to occupy the dimensions given by the vector. Note that
-- this does nothing prior to the 2014 versions of OpenSCAD.
resize :: Vector v => v -> Model v -> Model v
resize v = UnaryOp (Resize v)

-- | Rotate a 'Model' around the z-axis
rotate2d :: Double -> Model2d -> Model2d
rotate2d th = UnaryOp (Rotate2d th)

-- | Rotate a 'Model' by different amounts around each of the three axis.
rotate3d :: Vector3d -> Model3d -> Model3d
rotate3d v = UnaryOp (Rotate3d v)

-- | Translate a 'Model' along a 'Vector'.
translate :: Vector v => v -> Model v -> Model v
translate v = UnaryOp (Translate v)

-- | Mirror a 'Model' across a plane intersecting the origin.
mirror :: Vector v => v -> Model v -> Model v
mirror v = UnaryOp (Mirror v)

-- | Render a 'Model' in a specific color. This doesn't use the
-- OpenSCAD color model, but instead uses the 'Data.Colour' model. The
-- 'Graphics.OpenSCAD' module rexports 'Data.Colour.Names' so you can
-- conveniently say @'color' 'red' /'Model'/@.
color :: Vector v => Colour Double -> Model v -> Model v
color c = UnaryOp (Color c)

-- | Render a 'Model' in a transparent color. This uses the
-- 'Data.Colour.AlphaColour' color model.
transparent :: Vector v => AlphaColour Double -> Model v -> Model v
transparent ac = UnaryOp (Transparent ac)

-- | A 'translate' that just goes up, since those seem to be common.
up :: Double -> Model3d -> Model3d
up f = translate (0, 0, f)

-- Combinations

-- | Create the union of a list of 'Model's.
union :: Vector v => [Model v] -> Model v
union = ListOp Union

-- | Create the intersection of a list of 'Model's.
intersection :: Vector v => [Model v] -> Model v
intersection = ListOp Intersection

-- | The difference between two 'Model's.
difference :: Vector v => Model v -> Model v -> Model v
difference = BinaryOp Difference

-- | The Minkowski sum of a list of 'Model's.
minkowski :: Vector v => [Model v] -> Model v
minkowski = ListOp Minkowski

-- | The convex hull of a list of 'Model's.
hull :: Vector v => [Model v] -> Model v
hull = ListOp Hull

-- | 'render' does all the real work. It will walk the AST for a 'Model',
-- returning an OpenSCAD program in a 'String'.
render :: Vector v => Model v -> String
render = show . PP.pretty

instance Vector v => PP.Pretty (Model v) where
  pretty = \case
    Shape s -> PP.pretty s
    Solid s -> PP.pretty s
    UnaryOp op m -> PP.pretty op <+> renderSubModels [m]
    BinaryOp op m1 m2 -> PP.pretty op <+> renderSubModels [m1, m2]
    ListOp op ms -> PP.pretty op <+> PP.braces (PP.cat $ PP.pretty <$> ms)
    Projection c m -> renderOperator "projection" [namedArg "cut" $ renderBool c] <+> renderSubModels [m]
    LinearExtrude h t sc sl c f m ->
      renderOperator
        "linear_extrude"
        ( [ namedArg "height" $ PP.pretty h,
            namedArg "twist" $ PP.pretty t,
            namedArg "scale" $ PP.pretty $ rVector sc, -- FIXME
            namedArg "slices" $ PP.pretty sl,
            namedArg "convexity" $ PP.pretty c
          ]
            ++ facetsToArgs f
        )
        <+> renderSubModels [m]
    RotateExtrude c f m -> renderOperator "rotate_extrude" (namedArg "convexity" (PP.pretty c) : facetsToArgs f) <+> renderSubModels [m]
    Import f -> renderAction "import" [PP.pretty $ "\"" ++ f ++ "\""]
    Var facets ss -> renderOperator "let" (facetsToArgs facets) <+> renderSubModels ss

instance Vector v => PP.Pretty (UnaryOp v) where
  pretty = \case
    Scale v -> renderOperator "scale" [PP.pretty $ rVector v]
    Resize v -> renderOperator "resize" [PP.pretty $ rVector v]
    Translate v -> renderOperator "translate" [PP.pretty $ rVector v]
    Rotate2d v -> renderOperator "rotate" [PP.pretty $ rVector ((0, 0, v) :: Vector3d)]
    Rotate3d v -> renderOperator "rotate" [PP.pretty $ rVector v]
    Mirror v -> renderOperator "mirror" [PP.pretty $ rVector v]
    MultMatrix (a, b, c, d) ->
      let q2list (w, x, y, z) = [w, x, y, z]
       in renderOperator
            "multmatrix"
            [PP.align . PP.list $ PP.list . fmap PP.pretty . q2list <$> [a, b, c, d]]
    Color c ->
      let r = toSRGB c
       in renderOperator "color" [PP.pretty $ rVector (channelRed r, channelGreen r, channelBlue r)]
    Transparent c ->
      renderOperator
        "color"
        [PP.list $ PP.pretty <$> [channelRed r, channelGreen r, channelBlue r, a]]
      where
        r = toSRGB $ toPure c
        a = alphaChannel c
        toPure ac = if a > 0 then darken (recip a) (ac `over` black) else black
    Offset d j ->
      let join = case j of
            Bevel -> namedArg "join_type" "bevel"
            Round -> namedArg "join_type" "round"
            Miter l -> namedArg "miter_limit" $ PP.pretty l
       in renderOperator "offset" [namedArg "delta" $ PP.pretty d, join]

instance Vector v => PP.Pretty (BinaryOp v) where
  pretty = \case
    Difference -> renderOperator "difference" []

instance Vector v => PP.Pretty (ListOp v) where
  pretty = \case
    Union -> renderOperator "union" []
    Intersection -> renderOperator "intersection" []
    Minkowski -> renderOperator "minkowski" []
    Hull -> renderOperator "hull" []

instance PP.Pretty Shape where
  pretty = \case
    Rectangle r f -> renderAction "square" [PP.list $ PP.pretty <$> [r, f]]
    Circle r facets ->
      renderAction "circle" $ PP.pretty r : facetsToArgs facets
    Polygon c points paths ->
      renderAction
        "polygon"
        [ namedArg "points" . PP.list $ PP.pretty . rVector <$> points,
          namedArg "paths" $ PP.pretty paths,
          namedArg "convexity" $ PP.pretty c
        ]

renderAction :: PP.Doc ann -> [PP.Doc ann] -> PP.Doc ann
renderAction name args =
  name
    <> PP.align (PP.tupled args)
    <> PP.semi

renderOperator :: PP.Doc ann -> [PP.Doc ann] -> PP.Doc ann
renderOperator name args = name <> PP.align (PP.tupled args)

renderSubModels :: Vector v => [Model v] -> PP.Doc ann
renderSubModels = \case
  [m] -> PP.pretty m
  ms -> PP.braces . PP.cat $ PP.pretty <$> ms

renderBool :: Bool -> PP.Doc ann
renderBool b = if b then "true" else "false"

namedArg :: PP.Doc ann -> PP.Doc ann -> PP.Doc ann
namedArg name val = name <> "=" <> val

instance PP.Pretty Solid where
  pretty = \case
    Sphere x f ->
      renderAction "sphere" (PP.pretty x : facetsToArgs f)
    Box x y z ->
      renderAction "cube" [PP.list $ PP.pretty <$> [x, y, z]]
    Cylinder r h f ->
      renderAction "cylinder" $
        [namedArg "r" $ PP.pretty r, namedArg "h" $ PP.pretty h] ++ facetsToArgs f
    ObCylinder r1 h r2 f ->
      renderAction "cylinder" $
        [namedArg "r1" $ PP.pretty r1, namedArg "h" $ PP.pretty h, namedArg "r2" $ PP.pretty r2]
          ++ facetsToArgs f
    Polyhedron c ps ss ->
      let sides = case ss of
            Faces vs -> namedArg "faces" . PP.list $ PP.pretty <$> vs
            Triangles vs -> namedArg "triangles" . PP.list $ PP.pretty <$> vs
       in renderAction
            "polyhedron"
            [ namedArg "points" . PP.list $ PP.pretty . rVector <$> ps,
              sides,
              namedArg "convexity" $ PP.pretty c
            ]
    Surface f i c ->
      renderAction
        "surface"
        [ namedArg "file" $ PP.dquotes (PP.pretty f),
          namedArg "invert" $ renderBool i,
          namedArg "convexity" $ PP.pretty c
        ]

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
drawL = draw . ListOp Union

facetsToArgs :: Facets -> [PP.Doc ann]
facetsToArgs (Facets fa' fs' fn') =
  catMaybes
    [ namedArg "$fa" . PP.pretty <$> fa',
      namedArg "$fs" . PP.pretty <$> fs',
      namedArg "$fn" . PP.pretty <$> fn'
    ]

-- | 'var' uses @assign@ to set a 'Facet' variable for it's 'Model's.
var :: Facets -> [Model v] -> Model v
var = Var

-- | 'fa' is used to set the @$fa@ variable in a 'Facet' or 'var'.
fa :: Double -> Facets
fa a = Facets (Just a) Nothing Nothing

-- | 'fs' is used to set the @$fs@ variable in a 'Facet' or 'var'.
fs :: Double -> Facets
fs a = Facets Nothing (Just a) Nothing

-- | 'fn' is used to set the @$fn@ variable in a 'Facet' or 'var'.
fn :: Int -> Facets
fn a = Facets Nothing Nothing (Just a)

def :: Facets
def = Facets Nothing Nothing Nothing

-- And one last convenience function.

-- | Use 'diam' to turn a diameter into a radius for circles, spheres, etc.
diam :: Double -> Double
diam = (/ 2)

instance Semigroup Model2d where
  Shape (Rectangle 0 0) <> b = b
  a <> Shape (Rectangle 0 0) = a
  a <> b = union [a, b]

instance Semigroup Model3d where
  Solid (Box 0 0 0) <> b = b
  a <> Solid (Box 0 0 0) = a
  a <> b = union [a, b]

-- Now, let Haskell work it's magic
instance Monoid Model2d where
  mempty = Shape $ Rectangle 0 0
  mconcat [a] = a
  mconcat as = union as

instance Monoid Model3d where
  mempty = Solid $ Box 0 0 0
  mconcat [a] = a
  mconcat as = union as

-- | You can use '(#)' to write transformations in a more readable postfix form,
--   cube 3 # color red # translate (-3, -3, -3)
infixl 8 #

(#) :: a -> (a -> c) -> c
(#) = flip ($)
