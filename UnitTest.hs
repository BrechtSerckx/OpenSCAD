#!/usr/bin/env runghc

module Main where

import Control.DeepSeq
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import Test.HUnit.Tools
import Graphics.OpenSCAD
import Data.Colour (withOpacity)
import Data.List.NonEmpty (fromList)
import Data.Monoid ((<>), Monoid,mconcat, mempty, mappend)



assertError err code =
  assertRaises "Check error"  (ErrorCall err) . evaluate $ deepseq (show code) ()

sw = concat . words
st n e a = testCase n $ (sw $ render a) @?= (sw e)


{- About the test result values.

Running "cabal test" does not verify that the results do the intended
thing in OpenSCAD. Possibly we'll add shell tests for that at some
point, but not yet.

For now, if you change or add strings, please manually copy them into
OpenSCAD and make sure they do what you want the Model data structure
that they are testing does.
-}

tests = testGroup "Tests" [
  testGroup "3d-primitives" [
     testGroup "Spheres" [
        st "1" "sphere(1.0);"       $ sphere 1 def,
        st "2" "sphere(2.0,$fn=100);" (sphere 2 $ fn 100),
        st "3" "sphere(2.0,$fa=5.0);" (sphere 2 $ fa 5),
        st "4" "sphere(2.0,$fs=0.1);" (sphere 2 $ fs 0.1)
        ],

     testGroup "Boxes" [
       st "box"  "cube([1.0,2.0,3.0]);" $ box 1 2 3,
       st "cube" "cube([2.0,2.0,2.0]);" $ cube 2
       ],

     testGroup "Cylinders" [
       st "1" "cylinder(r=1.0,h=2.0);"         $ cylinder 1 2 def,
       st "2" "cylinder(r=1.0,h=2.0,$fs=0.6);"  (cylinder 1 2 $ fs 0.6),
       st "3" "cylinder(r=1.0,h=2.0,$fn=10);"   (cylinder 1 2 $ fn 10),
       st "4" "cylinder(r=1.0,h=2.0,$fa=30.0);" (cylinder 1 2 $ fa 30)
       ],

     testGroup "Oblique-Cylinders" [
       st "1" "cylinder(r1=1.0,h=2.0,r2=2.0);" $ obCylinder 1 2 2 def,
       st "2" "cylinder(r1=1.0,h=2.0,r2=2.0,$fs=0.6);"
          (obCylinder 1 2 2 $ fs 0.6),
       st "3" "cylinder(r1=1.0,h=2.0,r2=2.0,$fn=10);"
          (obCylinder 1 2 2 $ fn 10),
       st "4" "cylinder(r1=1.0,h=2.0,r2=2.0,$fa=30.0);"
          (obCylinder 1 2  2 $ fa 30)
       ],

     testGroup "Misc" [
       st "import" "import(\"test.stl\");" (solid $ importFile "test.stl"),
       st "polyhedron 1"
          "polyhedron(points=[[10.0,10.0,0.0],[10.0,-10.0,0.0],[0.0,0.0,10.0],[-10.0,-10.0,0.0],[-10.0,10.0,0.0]],triangles=[[0,1,2],[1,3,2],[3,4,2],[4,0,2],[1,0,4],[3,1,4]],convexity=1);" $
          polyhedron 1 [[(10, 10, 0), (10, -10, 0), (0, 0, 10)],
                        [(10, -10, 0), (-10, -10, 0), (0, 0, 10)],
                        [(-10, -10, 0), (-10, 10, 0), (0, 0, 10)],
                        [(-10, 10, 0), (10, 10, 0), (0, 0, 10)],
                        [(10, -10, 0), (10, 10, 0), (-10, 10, 0)],
                        [(-10, -10, 0), (10, -10, 0), (-10, 10, 0)]],
       st "polyhedron 2"
          "polyhedron(points=[[10.0,10.0,0.0],[10.0,-10.0,0.0],[0.0,0.0,10.0],[-10.0,-10.0,0.0],[-10.0,10.0,0.0]],faces=[[0,1,2],[1,3,2],[3,4,2],[4,0,2],[4,3,1,0]],convexity=1);" $
          polyhedron 1 [[(10, 10, 0), (10, -10, 0), (0, 0, 10)],
                        [(10, -10, 0), (-10, -10, 0), (0, 0, 10)],
                        [(-10, -10, 0), (-10, 10, 0), (0, 0, 10)],
                        [(-10, 10, 0), (10, 10, 0), (0, 0, 10)],
                        [(-10, 10, 0), (-10, -10, 0), (10, -10, 0), (10, 10, 0)]],
       st "unsafePolyhedron"
          "polyhedron(points=[[10.0,10.0,0.0],[10.0,-10.0,0.0],[-10.0,-10.0,0.0],[-10.0,10.0,0.0],[0.0,0.0,10.0]],faces=[[0,1,4],[1,2,4],[2,3,4],[3,0,4],[1,0,3],[2,1,3]],convexity=1);"
          (unsafePolyhedron 1 [(10.0,10.0,0.0),(10.0,-10.0,0.0),(-10.0,-10.0,0.0),
                               (-10.0,10.0,0.0),(0.0,0.0,10)]
                            $ Faces [[0,1,4],[1,2,4],[2,3,4],[3,0,4],[1,0,3],
                                     [2,1,3]])
       ],
     testGroup "Linear-Extrusion" [
       st "1" 
          "linear_extrude(height=10.0,twist=0.0,scale=[1.0,1.0],slices=10,convexity=10)circle(1.0);"
          (linearExtrude 10 0 (1, 1) 10 10 def $ circle 1 def),
       st "2" 
          "linear_extrude(height=10.0,twist=100.0,scale=[1.0,1.0],slices=10,convexity=10)translate([2.0,0.0])circle(1.0);"
          (linearExtrude 10 100 (1, 1) 10 10 def $ translate (2, 0)
           $ circle 1 def),
       st "3" 
          "linear_extrude(height=10.0,twist=500.0,scale=[1.0,1.0],slices=10,convexity=10)translate([2.0,0.0])circle(1.0);"
          (linearExtrude 10 500 (1, 1) 10 10 def $ translate (2, 0)
           $ circle 1 def),
       st "4" 
          "linear_extrude(height=10.0,twist=360.0,scale=[1.0,1.0],slices=100,convexity=10)translate([2.0,0.0])circle(1.0);"
          (linearExtrude 10 360 (1, 1) 100 10 def $ translate (2, 0)
           $ circle 1 def),
       st "5" 
          "linear_extrude(height=10.0,twist=360.0,scale=[1.0,1.0],slices=100,convexity=10,$fn=100)translate([2.0,0.0])circle(1.0);"
          (linearExtrude 10 360 (1, 1) 100 10 (fn 100) $ translate (2, 0)
           $ circle 1 def),
       st "6" 
          "linear_extrude(height=10.0,twist=0.0,scale=[3.0,3.0],slices=100,convexity=10)translate([2.0,0.0])circle(1.0);"
          (linearExtrude 10 0 (3, 3) 100 10 def $ translate (2, 0) $ circle 1 def),
       st "7" 
          "linear_extrude(height=10.0,twist=0.0,scale=[1.0,5.0],slices=100,convexity=10,$fn=100)translate([2.0,0.0])circle(1.0);"
          (linearExtrude 10 0 (1, 5) 100 10 (fn 100) $ translate (2, 0)
           $ circle 1 def)
       ],

     testGroup "Rotated-Extrusion" [
       st "1" "rotate_extrude(convexity=10)translate([2.0,0.0])circle(1.0);"
          (rotateExtrude 10 def $ translate (2, 0) $ circle 1 def),
       st "2"
          "rotate_extrude(convexity=10,$fn=100)translate([2.0,0.0])circle(1.0,$fn=100);"
          (rotateExtrude 10 (fn 100) $ translate (2, 0) $ circle 1 $ fn 100)
       ],

     testGroup "Surface" [
       st "Normal" "surface(file=\"test.dat\",convexity=5);" $
          surface "test.dat" False 5,
       st "Inverted" "surface(file=\"test.dat\",invert=true,convexity=5);" $
          surface "test.dat" True 5	-- Requires  2014.QX
       ]
     ],
     
  testGroup "2d-primitives" [
    testGroup "Squares" [
       st "rectangle" "square([2.0,3.0]);" $ rectangle 2 3,
       st "square"    "square([2.0,2.0]);" $ square 2
       ],
    testGroup "Circles" [
      st "1" "circle(1.0);"        $ circle 1 def,
      st "2" "circle(2.0,$fn=100);" (circle 2 $ fn 100),
      st "3" "circle(2.0,$fa=5.0);" (circle 2 $ fa 5),
      st "4" "circle(2.0,$fs=0.1);" (circle 2 $ fs 0.1)
      ],
    testGroup "Misc" [
      st "import" "import(\"test.dxf\");"   (solid $ importFile "test.dxf"),
      st "polygon"
         "polygon(points=[[0.0,0.0],[100.0,0.0],[0.0,100.0],[10.0,10.0],[80.0,10.0],[10.0,80.0]],paths=[[0,1,2],[3,4,5]],convexity=10);" $
         polygon 10 [[(0,0),(100,0),(0,100)],[(10,10),(80,10),(10,80)]],
      st "unsafePolygon"
         "polygon(points=[[0.0,0.0],[100.0,0.0],[0.0,100.0],[10.0,10.0],[80.0,10.0],[10.0,80.0]], paths=[[0,1,2],[3,4,5]],convexity=1);"
         (unsafePolygon 1 [(0,0),(100,0),(0,100),(10,10),(80,10),(10,80)]
                        [[0,1,2],[3,4,5]]),
      st "projection"
         "projection(cut=false)scale([10.0,10.0,10.0])difference(){translate([0.0,0.0,1.0])cube([1.0,1.0,1.0]);translate([0.25,0.25,0.0])cube([0.5,0.5,3.0]);}"
         (projection False . scale (10, 10, 10) . difference (up 1 (cube 1))
          $ translate (0.25, 0.25, 0) (box 0.5 0.5 3))
      ]
    ],

  testGroup "Transformations" [
    testGroup "Size changes" [
       st "scale 1" "scale([0.5,1.0,2.0])cube([1.0,1.0,1.0]);"
          (scale (0.5, 1, 2) $ cube 1),
       st "scale 2" "scale([0.5,2.0])square([1.0,1.0]);"
          (scale (0.5, 2) $ rectangle 1 1),
       st "resize 1" "resize([10.0,20.0])square([2.0,2.0]);"
          (resize (10, 20) $ square 2),
       st "resize 2" "resize([10.0,20.0,30.0])cube([2.0,2.0,2.0]);"
          (resize (10, 20, 30) $ cube 2)
       ],

    testGroup "Rotations" [
      st "1" "rotate([180.0,0.0,0.0])cube([2.0,2.0,2.0]);"
         (rotate (180, 0, 0) $ cube 2),
      st "2" "rotate([0.0,180.0,0.0])cube([2.0,2.0,2.0]);"
         (rotate (0, 180, 0) $ cube 2),
      st "3" "rotate([0.0,180.0,180.0])cube([2.0,2.0,2.0]);"
         (rotate (0, 180, 180) $ cube 2),
      st "4" "rotate([180.0,0.0])square([2.0,1.0]);"
         (rotate (180, 0) $ rectangle 2 1),
      st "5" "rotate([0.0,180.0])square([2.0,1.0]);"
         (rotate (0, 180) $ rectangle 2 1)
      ],
    testGroup "Mirrors" [
      st "1" "mirror([1.0,0.0,0.0])cube([2.0,2.0,2.0]);"
         (mirror (1, 0, 0) $ cube 2),
      st "2" "mirror([0.0,1.0,0.0])cube([2.0,2.0,2.0]);"
         (mirror (0, 1, 0) $ cube 2),
      st "3" "rotate([0.0,1.0,1.0])cube([2.0,2.0,2.0]);"
         (rotate (0, 1, 1) $ cube 2),
      st "4" "mirror([1.0,0.0])square([2.0,1.0]);"
         (mirror (1, 0) $ rectangle 2 1),
      st "2" "mirror([0.0,1.0])square([2.0,1.0]);"
         (mirror (0, 1) $ rectangle 2 1)
         ],
    
    st "multmatrix"
       "multmatrix([[1.0,0.0,0.0,10.0],[0.0,1.0,0.0,20.0],[0.0,0.0,1.0,30.0],[0.0,0.0,0.0,1.0]])cylinder(r=2.0,h=3.0);"
       (multMatrix ( (1, 0, 0, 10),
                     (0, 1, 0, 20),
                     (0, 0, 1, 30),
                     (0, 0, 0,  1) ) $ cylinder 2 3 def),

    testGroup "Colors" [
      st "color 1" "color([1.0,0.0,0.0])cube([1.0,1.0,1.0]);" (color red $ cube 1),
      st "color 2" "color([1.0,0.0,0.0])square([1.0,1.0]);"
         (color red $ square 1),
      st "transparent 1" "color([1.0,0.0,0.0,0.7])cube([1.0,1.0,1.0]);"
         (transparent (red `withOpacity` 0.7) $ cube 1),
      st "transparent 2" "color([1.0,0.0,0.0,0.7])square([1.0,1.0]);"
         (transparent (red `withOpacity` 0.7) $ square 1)
      ]
    ],

  testGroup "Facets" [
    st "facet 1" "assign($fn=100){sphere(2.0,$fn=100);}"
       (var (fn 100) [sphere 2 $ fn 100]),
    st "facet 2" "assign($fa=5.0){sphere(2.0,$fa=5.0);}"
       (var (fa 5) [sphere 2 $ fa 5]),
    st "facet 3" "assign($fs=0.1){sphere(2.0,$fs=0.1);}"
       (var (fs 0.1) [sphere 2 $ fs 0.1])
    ],

  testGroup "Errors" [
    testCase "Polygon Pointcount"
    . assertError "Polygon has fewer than 3 points." $
        polygon 1 [[(0, 0), (0, 1)]],
    testCase "Polygon Linearity"
    . assertError "Points in polygon are collinear." $
        polygon 1 [[(0, 0), (0, 1), (0, 2)]],
    testCase "Polyhedron Linearity"
    . assertError "Some face has collinear points." $
        polyhedron 1 [[(0, 0, 0), (1, 0, 0), (2, 0, 0)]],
    testCase "Polyhedron Planarity" . assertError "Some face isn't coplanar." $
        polyhedron 1 [[(10, 10, 0), (10, -10, 0), (0, 10, 10)],
                        [(10, -10, 0), (-10, -10, 0), (0, 0, 10)],
                        [(-10, -10, 0), (-10, 10, 0), (0, 0, 10)],
                        [(-10, 10, 0), (10, 10, 0), (0, 0, 10)],
                        [(-10, 10, 0), (-10, -10, 0), (10, -10, 0), (0, 0, -10)]],
    testCase "Polyhedron Edges" . assertError "Some edges are not in two faces." $
        polyhedron 1 [[(10, 10, 0), (10, -10, 0), (0, 0, 10)],
                      [(10, -10, 0), (-10, -10, 0), (0, 0, 10)],
                      [(-10, -10, 0), (-10, 10, 0), (0, 0, 10)],
                      [(-10, 10, 0), (10, 10, 0), (0, 0, 10)],
                      [(10, -10, 0), (10, 10, 0), (-10, 10, 0)],
                      [(-10, -10, 0), (10, -10, 0), (-10, 20, 0)]],
    testCase "Polyhedron Faces"
    . assertError "Some faces have different orientation." $
        polyhedron 1 [[(10, 10, 0), (10, -10, 0), (0, 0, 10)],
                      [(10, -10, 0), (-10, -10, 0), (0, 0, 10)],
                      [(-10, -10, 0), (-10, 10, 0), (0, 0, 10)],
                      [(-10, 10, 0), (10, 10, 0), (0, 0, 10)],
                      [(10, -10, 0), (10, 10, 0), (-10, 10, 0)],
                      [(10, -10, 0), (-10, -10, 0), (-10, 10, 0)]],
    testCase "Polyhedron Orientation"
    . assertError "Face orientations are counterclockwise." $
        polyhedron 1 [[(10, -10, 0), (10, 10, 0), (0, 0, 10)],
                      [(-10, -10, 0), (10, -10, 0), (0, 0, 10)],
                      [(-10, 10, 0), (-10, -10, 0), (0, 0, 10)],
                      [(10, 10, 0), (-10, 10, 0), (0, 0, 10)],
                      [(10, 10, 0), (10, -10, 0), (-10, 10, 0)],
                      [(10, -10, 0), (-10, -10, 0), (-10, 10, 0)]]
    ],

  testGroup "Combinations" [
    st "union" "union(){cube([1.0,1.0,1.0]);sphere(1.1,$fs=0.1);}"
       (union [cube 1, sphere 1.1 $ fs 0.1]),
    st "difference" "difference(){cube([1.0,1.0,1.0]);sphere(1.1,$fs=0.1);}"
       (difference (cube 1) . sphere 1.1 $ fs 0.1),
    st "intersection" "intersection(){cube([1.0,1.0,1.0]);sphere(1.1,$fs=0.1);}"
       (intersection [cube 1, sphere 1.1 $ fs 0.1]),
    st "minkowski"
       "minkowski(){cube([10.0,10.0,10.0]);cylinder(r=2.0,h=1.1,$fn=50);}"
       (minkowski [cube 10, cylinder 2 1.1 $ fn 50]),
    st "hull" "hull(){translate([15.0,10.0])circle(10.0);circle(10.0);}"
       (hull [circle 10 def # translate (15, 10), circle 10 def])
    ],

  testGroup "Haskell" [
    st "# 3d" "translate([-3.0,-3.0,-3.0])color([1.0,0.0,0.0])cube([3.0,3.0,3.0]);"
       (cube 3 # color red # translate (-3, -3, -3)),
    st "# 2d"
       "translate([3.0,3.0])color([1.0,0.6470588235294119,0.0])square([2.0,2.0]);"
       (square 2 # color orange # translate (3, 3)),
    st "Monoid 1 3d" "union(){cube([1.0,1.0,1.0]);sphere(1.1,$fs=0.1);}"
       (cube 1 <> sphere 1.1 (fs 0.1)),
    st "Monoid 1 2d" "union(){square([1.0,1.0]);circle(1.1,$fs=0.1);}"
       (square 1 <> circle 1.1 (fs 0.1)),
    st "Monoid 2 3d" "union(){cube([1.0,1.0,1.0]);sphere(1.1,$fs=0.1);}"
       (mconcat [cube 1, sphere 1.1 $ fs 0.1]),
    st "Monoid 2 2d" "union(){square([1.0,1.0]);circle(1.1,$fs=0.1);}"
       (mconcat [square 1, circle 1.1 $ fs 0.1]),
    st "Monoid 3 3d" "sphere(1.1,$fs=0.1);" (mconcat [sphere 1.1 $ fs 0.1]),
    st "Monoid 3 2d" "square([1.0,1.0]);" (mconcat [square 1]),
    st "Monoid 4 3d" "cube([0.0,0.0,0.0]);" (solid mempty),
    st "Monoid 4 2d" "cube([0.0,0.0,0.0]);" (mempty :: Model2d),
    st "Monoid 5 3d" "union(){cube([1.0,1.0,1.0]);sphere(1.1,$fs=0.1);}"
       (mappend (cube 1) $ sphere 1.1 (fs 0.1)),
    st "Monoid 5 2d" "union(){square([1.0,1.0]);circle(1.1,$fs=0.1);}"
       (mappend (square 1) $ circle 1.1 (fs 0.1))
    ]
  ]

main = defaultMain tests
