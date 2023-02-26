import Criterion.Main
import Graphics.OpenSCAD
import System.Process

renderScadS :: FilePath -> String -> String -> IO String
renderScadS basename ext s = do
  writeFile (basename <> ".scad") s
  readProcess "openscad" ["-q", "-o", basename <> "." <> ext, "-"] s

benchScadS :: String -> String -> String -> Benchmark
benchScadS name ext =
  bench name
    . nfIO
    . renderScadS ("bench/out/" <> name) ext

benchScadSGroup ::
  Vector v =>
  [(String, Int -> (Int -> v) -> Model v -> String)] ->
  String ->
  String ->
  Int ->
  (Int -> v) ->
  Model v ->
  Benchmark
benchScadSGroup renderFs group ext n mkTranslation model =
  bgroup
    group
    [ benchScadS (group <> "." <> name) ext $
        renderF n mkTranslation model
      | (name, renderF) <- renderFs
    ]

renderAsUnion :: Vector v => Int -> (Int -> v) -> Model v -> String
renderAsUnion n f m = render $ union [translate (f i) m | i <- [0 .. n]]

renderAsModule :: Vector v => Int -> (Int -> v) -> Model v -> String
renderAsModule n f m =
  unlines $
    ["module foo () {", render m, "}"]
      <> ["translate(" <> rVector (f i) <> ") { foo(); };" | i <- [0 .. n]]

benchUnionModule ::
  Vector v => String -> String -> Int -> (Int -> v) -> Model v -> Benchmark
benchUnionModule = benchScadSGroup [("union", renderAsUnion), ("module", renderAsModule)]

renderAsExtrudedUnion :: Int -> (Int -> Vector2d) -> Model2d -> String
renderAsExtrudedUnion n f m = render . linearExtrude 10 0 (1, 1) 0 0 def $ union [translate (f i) m | i <- [0 .. n]]

renderAsExtrudedModule :: Int -> (Int -> Vector2d) -> Model2d -> String
renderAsExtrudedModule n f m =
  unlines $
    ["module foo () {", render m, "}", "linear_extrude(10) {"]
      <> ["translate(" <> rVector (f i) <> ") { foo(); };" | i <- [0 .. n]]
      <> ["}"]

benchExtrudedUnionModule ::
  String -> String -> Int -> (Int -> Vector2d) -> Model2d -> Benchmark
benchExtrudedUnionModule = benchScadSGroup [("union", renderAsExtrudedUnion), ("module", renderAsExtrudedModule)]

main = do
  defaultMain
    [ benchUnionModule "square" "svg" 100 (\i -> (fromIntegral i * 11, 0)) $
        square 10,
      benchUnionModule "cube" "stl" 100 (\i -> (fromIntegral i * 11, 0, 0)) $ cube 10,
      benchUnionModule "complex square" "svg" 100 (\i -> (fromIntegral i * 21, 0)) $
        foldr difference (square 20) [square i | i <- reverse [1 .. 19]],
      benchUnionModule "complex cube" "stl" 20 (\i -> (fromIntegral i * 21, 0, 0)) $
        foldr difference (cube 20) [cube i | i <- reverse [1 .. 19]],
      benchExtrudedUnionModule "extruded square" "stl" 100 (\i -> (fromIntegral i * 11, 0)) $
        square 10,
      benchExtrudedUnionModule "extruded complex square" "stl" 20 (\i -> (fromIntegral i * 21, 0)) $
        foldr difference (square 20) [square i | i <- reverse [1 .. 19]]
    ]
