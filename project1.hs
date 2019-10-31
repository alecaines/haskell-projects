{-
  Name: Alexander Caines
  Project: Project1

  Defining and testing some functions in Haskell
-}

module Project1 where

--Function that returns the exponentiation of a given base and exponent
power :: Integer -> Integer -> Integer
power base exponent
    | exponent == 0 = 1
    | odd exponent = base * (power base (exponent - 1))
    | otherwise = (power base (exponent `div` 2 )) ^ 2

--tail-recursive function that returns the distance traveled by a bouncing ball
bouncy :: Float -> Float -> Integer -> Float -> Float
bouncy height bounciness 0 sum = sum
bouncy height bounciness multiplicity sum = (bouncy (height * bounciness) bounciness (multiplicity - 1)) (sum + (height + (height * bounciness)))

--Returns a recalibrates the approximation for the square root found in the newton method
improve :: Float -> Float -> Float
improve x z = ((z + (x / z)) / 2)

{-Returns a boolean that confirms that the difference between the approximation of
a number's square root and it's actual square root is within a given margin-}
goodEnough :: Float -> Float -> Float -> Bool
goodEnough x z tolerance = tolerance > abs((z ^ 2) - x)

--Returns an approximation to the square root of a given number
newton :: Float -> Float -> Float -> Float
newton x z tolerance
    | goodEnough x z tolerance = z
    | otherwise = newton x (improve x z) tolerance
