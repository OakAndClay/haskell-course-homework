-- Question 1
-- Write a function that checks if the monthly consumption of an electrical device is bigger, equal, or smaller than the maximum allowed and
-- returns a message accordingly. 
-- The function has to take the hourly consumption of an electrical device, the hours of daily use, and the maximum monthly consumption allowed.
-- (Monthly usage = consumption (kW) * hours of daily use (h) * 30 days).
checkConsumption :: (Ord a, Show a, Num a) => a -> a -> a -> String
checkConsumption c h m 
    | GT == compare monthlyUsage m = "You need to cut back a little."
    | LT == compare monthlyUsage m = "Looking Good!"
    | otherwise                    = "Take it easy. Your at the limit."
        where 
            monthlyUsage = c * h * 30

-- Question 2
-- Prelude:
-- We use the function `show :: a -> String` to transform any type into a String.
-- So `show 3` will produce `"3"` and `show (3 > 2)` will produce `"True"`.

-- In the previous function, return the excess/savings of consumption as part of the message.
checkConsumption' :: (Ord a, Show a, Num a) => a -> a -> a -> String
checkConsumption' c h m 
    | GT == compare monthlyUsage m = "You need to cut back a little. You have used " ++ usageDifference ++ " kW more than the limit"
    | LT == compare monthlyUsage m = "Looking Good! " ++ usageDifference ++ " kW remaining."
    | otherwise                    = "Take it easy. Your at the limit."
        where 
            monthlyUsage = c * h * 30
            usageDifference = show . abs $ m - monthlyUsage

-- Question 3
-- Write a function that showcases the advantages of using let expressions to split a big expression into smaller ones.
-- Then, share it with other students in Canvas.
{-
Write a function that takes in the distance to destination in miles (d) the amount of fuel in the vehicle in gallons (g) the average speed of travel 
in mph (s) and the mpg (m) and returns a response for the possible outcomes
-Have enough fuel and arrive in about _ minutes with _ gallons remaining
-Don't have enough fuel and need to refuel within the next _ minutes 
-Might make it there
-}

fuelCheck :: (Ord a, Show a, Num a, Fractional a, RealFrac a) => a -> a -> a -> a -> String
fuelCheck d g s m = 
    let 
        fuelSurplus     = show $ g - galToDest
        distanceToEmpty = g * m
        galToDest       = (d / m)
        timeToArrival   = show $ round (d / s) * 60
        timeToEmpty     = show $ round (distanceToEmpty / s) * 60
    in 
        if d < distanceToEmpty   
            then "You have enough fuel and you will arive in about " ++ timeToArrival ++ " minutes. With " ++ fuelSurplus ++ " gallons remaining."
            else
                if d > distanceToEmpty   
                    then "You need to re-fuel. In about " ++ timeToEmpty ++ " minutes you will run out of gas."
                    else "You might make it there. Be careful."

fuelCheck' :: (Ord a, Show a, Num a, Fractional a, RealFrac a) => a -> a -> a -> a -> String
fuelCheck' d g s m
    | d < distanceToEmpty   = "You have enough fuel and you will arive in about " ++ timeToArrival ++ " minutes. With " ++ fuelSurplus ++ " gallons remaining."
    | d > distanceToEmpty   = "You need to re-fuel. In about " ++ timeToEmpty ++ " minutes you will run out of gas."
    | otherwise             = "You might make it there. Be careful."
        where
            fuelSurplus     = show $ g - galToDest
            distanceToEmpty = g * m
            galToDest       = (d / m)
            timeToArrival   = show $ round (d / s) * 60
            timeToEmpty     = show $ round (distanceToEmpty / s) * 60


-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  
q4 :: (Ord a, Show a, Fractional a) => a -> a -> String
q4 x y = 
    if x == 0 || y == 0
        then "This isn't possible"
        else
            if x > y
                then show $ y / x
                else    
                     show $ x / y

q4' :: (Ord a, Show a, Fractional a) => a -> a -> String
q4' x y
    | x == 0 || y == 0 = "This isn't possible"
    | x > y            = show $ y / x
    | otherwise        = show $ x / y

q4'' :: (Ord a, Show a, Fractional a) => a -> a -> String
q4'' x y
    | x > y     = if x /= 0 then show $ y / x else "x is greater but 0"
    | x < y     = if y /= 0 then show $ x / y else "y is greater but 0"
    | otherwise = if x /= 0 then "1" else "x and y are both zero"

-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.

sumSquares :: Floating a => a -> a -> a
sumSquares x y =    let prodSqrt = sqrt xyProd where xyProd = x * y 
                    in prodSqrt + quotSqrt 
                    where quotSqrt = let xyQuot = x / y in sqrt xyQuot