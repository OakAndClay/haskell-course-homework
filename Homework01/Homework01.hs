
-- Question 1
-- Write a multiline comment below.
{-
This is a multiline comment. 
It is on multiple lines.
-}

-- Question 2
-- Define a function that takes a value and multiplies it by 3.
mult3 :: Floating a => a -> a
mult3 x = 3 * x

-- Question 3
-- Define a function that calculates the area of a circle.
circleArea :: Floating a => a -> a
circleArea r = pi * r ^ 2

-- Question 4
-- Define a function that calculates the volume of a cylinder by composing the previous function together with the height of the cylinder. 
-- cylVolume :: Floating a => a -> a
cylVolume :: Floating a => a -> a -> a
cylVolume h r = circleArea r * h

-- Question 5
-- Define a function that takes the height and radius of a cylinder and checks if the volume is greater than or equal to 42.

checkVol42 :: (Ord a, Floating a) => a -> a -> Bool
checkVol42 h r = cylVolume h r > 42