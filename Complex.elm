-- This is from the TheSeamau5 on GitHub, but doesn't seem to 
-- be installable by elm-package.
-- (https://github.com/TheSeamau5/Complex/blob/master/Complex.elm)


-- *Small* modifications were made on my part to make it compatible with my version of Elm.

module Complex where

{-| A library for complex numbers. The complex numbers
are represented by a record with two Float fields "real"
and "imaginary".

#Types
@docs Complex

#Creating Complex numbers
The simplest way to create a complex number is by using
type constructor for the Complex type. To create the
complex number 2 + 3i :

    x = Complex 2 3

The first argument sets the real part and the second sets
the imaginary part.

#Conversions
@docs toComplex, toImaginary, fromTuple, toTuple, toPolar

#Common values
@docs i, j

#Common Operations on Complex Numbers
@docs cAdd, cSub, cNeg, cMul, cDiv, cScaleBy, cSqrt

#Conjugate
@docs conjugate

#Properties of Complex Numbers
@docs cAbs, cArg

#Exponentiation
@docs cExp, cPow, cLn

#Trigonometric Functions
@docs cSin, cCos, cTan, cSec, cCsc, cCot

-}


{-| The Complex type. Used to represent a complex number
with a real part and an imaginary part.
-}
type alias Complex = { real : Float
               , imaginary : Float }


{-| Adds two complex numbers and returns the result.

      (Complex 1 2) `cAdd` (Complex 3 4) == Complex 4 6
-}
cAdd : Complex -> Complex -> Complex
cAdd z w = Complex (z.real + w.real)
                   (z.imaginary + w.imaginary)

{-| Subtracts two complex numbers and returns the result.

      (Complex 1 2) `cSub` (Complex 3 4) == Complex -2 -2
-}
cSub : Complex -> Complex -> Complex
cSub z w = Complex (z.real - w.real)
                   (z.imaginary - w.imaginary)

{-| Negates a complex number
      
      cNeg (Complex 1 2) == Complex -1 -2
-}
cNeg : Complex -> Complex 
cNeg z = Complex (-z.real) (-z.imaginary)


{-| Multiplies two complex numbers and returns the result

      (Complex 2 3) `cMul` (Complex 4 1) == Complex 5 14

-}
cMul : Complex -> Complex -> Complex
cMul z w = 
  let a = z.real
      b = z.imaginary
      c = w.real
      d = w.imaginary
  in Complex (a * c - b * d)
             (b * c + a * d)

{-| Scales a complex number by some Float
(i.e. multiplies a real number by a complex number)

      cScaleBy 2 (Complex 1 3) == Complex 2 6

-}
cScaleBy : Float -> Complex -> Complex
cScaleBy n z = Complex (z.real * n) (z.imaginary * n)

{-| Divides two complex numbers and returns the result

      (Complex 1 2) `cDiv` (Complex 2 2) == Complex 0.75 0.25 

-}
cDiv : Complex -> Complex -> Complex
cDiv z w =
  let a = z.real
      b = z.imaginary
      c = w.real
      d = w.imaginary
      denom = c * c + d * d 
  in Complex ((a * c + b * d) / denom)
             ((b * c - a * d) / denom)

{-| Returns the conjugate of a Complex number

      conjugate (Complex 1 2) == Complex 1 -2

-}
conjugate : Complex -> Complex
conjugate z = Complex z.real (-z.imaginary)


{-| Returns the absolute value of a Complex number 
as a Float.

      cAbs (Complex 9 12) == 15

-}
cAbs : Complex -> Float
cAbs z = 
  let a = z.real
      b = z.imaginary
  in sqrt (a * a + b * b)

{-| Returns the argument or phase of a complex number.
The argument is the angle of the radius (line segment
from the origin to the complex number) with the positive
real axis.

      cArg (Complex 1 1) == pi / 4

-}
cArg : Complex -> Float
cArg z = atan2 z.imaginary z.real 

{-| Returns the principal square root of a complex number

      cSqrt (Complex 3 4) == Complex 2 1

-}
cSqrt : Complex -> Complex
cSqrt z = 
  let signum x =  if  x == 0 then  0
                  else if  x <  0 then -1
                  else 1
      a = z.real
      b = z.imaginary
      c = sqrt ((a + sqrt (a * a + b * b)) / 2)
      d = (signum b) * sqrt ((-a + sqrt (a * a + b * b)) / 2)
  in Complex c d


{-| Return the natural logarithm of a complex number
-}
cLn : Complex -> Complex 
cLn z = Complex (logBase e (cAbs z)) (cArg z)

{-| Return e raised to a complex power
      
      cExp (Complex 0 pi) == Complex -1 0
      -- If you don't think that the above is beautiful
      -- then you have no emotion
-}
cExp : Complex -> Complex
cExp z = 
  let a = z.real
      b = z.imaginary
  in Complex (e^a * cos b) (e^a * sin b)

{-| Return the result of raising a complex number to a 
complex power
-}
cPow : Complex -> Complex -> Complex
cPow base exponent =  cExp ( exponent `cMul` (cLn base) )

{-| Returns the sine of a complex number
-}
cSin : Complex -> Complex
cSin z = 
  let iz = i `cMul` z
      niz = cNeg iz
  in cExp iz `cSub` cExp niz `cDiv` cScaleBy 2 i

{-| Returns the cosine of a complex number
-}
cCos : Complex -> Complex
cCos z = 
  let iz = i `cMul` z
      niz = cNeg iz
  in cScaleBy 0.5 (cExp iz `cAdd` cExp niz)

{-| Returns the tangent of a complex number
-}
cTan : Complex -> Complex
cTan z = cSin z `cDiv` cCos z 

{-| Returns the secant of a complex number
-}
cSec : Complex -> Complex
cSec z = toComplex 1 `cDiv` cCos z

{-| Returns the cosecant of a complex number
-}
cCsc : Complex -> Complex
cCsc z = toComplex 1 `cDiv` cSin z

{-| Returns the cotangent of a complex number
-}
cCot : Complex -> Complex
cCot z = toComplex 1 `cDiv` cTan z

{-| Converts a Float to the Complex type by setting the 
real part.

      toComplex 3 == Complex 3 0
-}
toComplex : Float -> Complex 
toComplex x = Complex x 0


{-| Converts a Float to the Complex type by setting the
imaginary part.

      toImaginary 3 == Complex 0 3
-}
toImaginary : Float -> Complex
toImaginary x = Complex 0 x

{-| Converts a 2-tuple of Floats to a complex number
      
      fromTuple (2,3) == Complex 2 3
-}
fromTuple : (Float , Float) -> Complex
fromTuple (real , imaginary) = Complex real imaginary

{-| Converts a Complex number to a 2-tuple of Floats
      
      toTuple (Complex 1 8) == (1,8)
-}
toTuple : Complex -> (Float, Float)
toTuple z = (z.real , z.imaginary)

{-| Converts a Complex number to a 2-tuple of Floats
in polar form
      
      toPolar i == (1 , pi / 2)
-}
toPolar : Complex -> (Float , Float)
toPolar z = (cAbs z , cArg z)

{-| Imaginary number
-}
i : Complex 
i = Complex 0 1

{-| Imaginary number
-}
j : Complex
j = i 