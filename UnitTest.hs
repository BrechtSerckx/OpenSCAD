module Main where

import Data.Monoid
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit

import Graphics.OpenSCAD
import Data.Colour (withOpacity)

sw = concat . words
st n e a = testCase n $ (sw e) @=?(sw $ render a)

main = defaultMainWithOpts [
  st "sphere 1"     "sphere(1.0);"                    (sphere 1 def),
  st "sphere 2"     "sphere(2.0,$fn=100);"            (sphere 2 $ fn 100),
  st "sphere 3"     "sphere(2.0,$fa=5.0);"            (sphere 2 $ fa 5),
  st "sphere 4"     "sphere(2.0,$fs=0.1);"            (sphere 2 $ fs 0.1),
  st "box"          "cube([1.0,2.0,3.0]);"            (box 1 2 3),
  st "cube"         "cube([2.0,2.0,2.0]);"            (cube 2),
  st "cylinder 1"   "cylinder(r=1.0,h=2.0);"          (cylinder 1 2 def),
  st "cylinder 2"   "cylinder(r=1.0,h=2.0,$fs=0.6);"  (cylinder 1 2 $ fs 0.6),
  st "cylinder 3"   "cylinder(r=1.0,h=2.0,$fn=10);"   (cylinder 1 2 $ fn 10),
  st "cylinder 4"   "cylinder(r=1.0,h=2.0,$fa=30.0);" (cylinder 1 2 $ fa 30),
  st "obCylinder 1" "cylinder(r1=1.0,h=2.0,r2=2.0);"  (obCylinder 1 2 2 def),
  st "obCylinder 2" "cylinder(r1=1.0,h=2.0,r2=2.0,$fs=0.6);"
     (obCylinder 1 2 2 $ fs 0.6),
  st "obCylinder 3" "cylinder(r1=1.0,h=2.0,r2=2.0,$fn=10);"
     (obCylinder 1 2 2 $ fn 10),
  st "obCylinder 4" "cylinder(r1=1.0,h=2.0,r2=2.0,$fa=30.0);"
     (obCylinder 1 2  2 $ fa 30),
  -- polyhedron & 3d import goes here
  st "rectangle"    "square([2.0,3.0]);"              (rectangle 2 3),
  st "square"       "square([2.0,2.0]);"              (square 2),
  st "circle 1"     "circle(1.0);"                    (circle 1 def),
  st "circle 2"     "circle(2.0,$fn=100);"            (circle 2 $ fn 100),
  st "circle 3"     "circle(2.0,$fa=5.0);"            (circle 2 $ fa 5),
  st "circle 4"     "circle(2.0,$fs=0.1);"            (circle 2 $ fs 0.1),
  -- polygon & 2d import goes here
  st "projection"   "projection(cut=false)scale([10.0,10.0,10.0])difference(){translate([0.0,0.0,1.0])cube([1.0,1.0,1.0]);translate([0.25,0.25,0.0])cube([0.5,0.5,3.0]);}"
     (projection False $ scale (10, 10, 10) $ difference (up 1 (cube 1)) $ translate (0.25, 0.25, 0) (box 0.5 0.5 3)),
  -- Transformations
  st "scale"        "scale([0.5,1.0,2.0])cube([1.0,1.0,1.0]);"
     (scale(0.5, 1, 2) $ cube 1),
  -- resize goes here
  st "rotate 1" "rotate(a=[180.0,0.0,0.0])cube([2.0,2.0,2.0]);"
     (rotate (180, 0, 0) $ cube 2),
  st "rotate 2" "rotate(a=[0.0,180.0,0.0])cube([2.0,2.0,2.0]);"
     (rotate (0, 180, 0) $ cube 2),
  st "rotate 3" "rotate(a=[0.0,180.0,180.0])cube([2.0,2.0,2.0]);"
     (rotate (0, 180, 180) $ cube 2),
  st "mirror 1" "mirror([1.0,0.0,0.0])cube([2.0,2.0,2.0]);"
     (mirror (1, 0, 0) $ cube 2),
  st "mirror 2" "mirror([0.0,1.0,0.0])cube([2.0,2.0,2.0]);"
     (mirror (0, 1, 0) $ cube 2),
  st "mirror 3" "rotate(a=[0.0,1.0,1.0])cube([2.0,2.0,2.0]);"
     (rotate (0, 1, 1) $ cube 2),
  st "multmatrix" "multmatrix([[1.0,0.0,0.0,10.0],[0.0,1.0,0.0,20.0],[0.0,0.0,1.0,30.0],[0.0,0.0,0.0,1.0]])cylinder(r=2.0,h=3.0);"
     (multMatrix ( (1, 0, 0, 10),
                   (0, 1, 0, 20),
                   (0, 0, 1, 30),
                   (0, 0, 0,  1) ) $ cylinder 2 3 def),
  st "color" "color([1.0,0.0,0.0])cube([1.0,1.0,1.0]);" (color red $ cube 1),
  st "transparent" "color([1.0,0.0,0.0,0.7])cube([1.0,1.0,1.0]);"
     (transparent (red `withOpacity` 0.7) $ cube 1),
  st "linearExtrude 1"
     "linear_extrude(height=10.0,twist=0.0,scale=[1.0,1.0],slices=10,convexity=10)circle(1.0);"
     (linearExtrude 10 0 (1, 1) 10 10 def $ circle 1 def),
  st "linearExtrude 2"
     "linear_extrude(height=10.0,twist=100.0,scale=[1.0,1.0],slices=10,convexity=10)translate([2.0,0.0])circle(1.0);"
     (linearExtrude 10 100 (1, 1) 10 10 def $ translate (2, 0) $ circle 1 def),
  st "linearExtrude 3"
     "linear_extrude(height=10.0,twist=500.0,scale=[1.0,1.0],slices=10,convexity=10)translate([2.0,0.0])circle(1.0);"
     (linearExtrude 10 500 (1, 1) 10 10 def $ translate (2, 0) $ circle 1 def),
  st "linearExtrude 4"
     "linear_extrude(height=10.0,twist=360.0,scale=[1.0,1.0],slices=100,convexity=10)translate([2.0,0.0])circle(1.0);"
     (linearExtrude 10 360 (1, 1) 100 10 def $ translate (2, 0) $ circle 1 def),
  st "linearExtrude 5"
     "linear_extrude(height=10.0,twist=360.0,scale=[1.0,1.0],slices=100,convexity=10,$fn=100)translate([2.0,0.0])circle(1.0);"
     (linearExtrude 10 360 (1, 1) 100 10 (fn 100) $ translate (2, 0) $ circle 1 def),
  st "linearExtrude 6"
     "linear_extrude(height=10.0,twist=0.0,scale=[3.0,3.0],slices=100,convexity=10)translate([2.0,0.0])circle(1.0);"
     (linearExtrude 10 0 (3, 3) 100 10 def $ translate (2, 0) $ circle 1 def),
  st "linearExtrude 7"
     "linear_extrude(height=10.0,twist=0.0,scale=[1.0,5.0],slices=100,convexity=10,$fn=100)translate([2.0,0.0])circle(1.0);"
     (linearExtrude 10 0 (1, 5) 100 10 (fn 100) $ translate (2, 0)
      $ circle 1 def),
  st "rotate_extrude 1"
     "rotate_extrude(convexity=10)translate([2.0,0.0])circle(1.0);"
     (rotateExtrude 10 def $ translate (2, 0) $ circle 1 def),
  st "rotate_extrude 2"
     "rotate_extrude(convexity=10,$fn=100)translate([2.0,0.0])circle(1.0,$fn=100);"
     (rotateExtrude 10 (fn 100) $ translate (2, 0) $ circle 1 $ fn 100),
  st "facet 1" "assign($fn=100){sphere(2.0,$fn=100);}"
     (var (fn 100) [sphere 2 $ fn 100]),
  st "facet 2" "assign($fa=5.0){sphere(2.0,$fa=5.0);}"
     (var (fa 5) [sphere 2 $ fa 5]),
  st "facet 3" "assign($fs=0.1){sphere(2.0,$fs=0.1);}"
     (var (fs 0.1) [sphere 2 $ fs 0.1])
  ] mempty
