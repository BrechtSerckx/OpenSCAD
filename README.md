# What's Graphics.OpenSCAD

This is a library whose primary component is an algebraic data type
for describing [OpenSCAD](http://openscad.org) models, and a function
that converts that data type into a string. There are convenience
functions to make building the model easier.

## What's different

Given the primitive and quirky nature of the OpenSCAD language, the
idea of "using OpenSCAD" as an assembler is both obvious, and promoted
in lieu of adding major extensions to OpenSCAD. So there are a number
of such projects, for a variety of languages.

Any compiler that generated "assembler" that caused the assembler
program to generate errors would be considered buggy. However, none of
the alternative projects I looked at seemed to do anything about that
(my apologies if I missed one - I only looked at languages I was
interested in using). Graphics.OpenSCAD takes the attitude that errors
from OpenSCAD on the generated code are errors in
Graphics.OpenSCAD. If you manage to generate code that causes OpenSCAD
to issue an error message, please open an issue here.

## More info

Read the
[online docs](https://hackage.haskell.org/package/OpenSCAD-0.2.1.0/docs/Graphics-OpenSCAD.html)
at [hackage](http://hackage.haskell.org/).
