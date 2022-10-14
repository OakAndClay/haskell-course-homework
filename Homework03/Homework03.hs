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
Write a program that takes in the distance to destination in miles (d) the amount of fuel in the vehicle in gallons (g) the average speed of travel 
in mph (s) and the mpg (m) and returns a response for the possible outcomes
Have enough fuel and arrive in the next _ minutes
Don't have enough fuel and need to refuel within the next _ minutes 
-}

fuelCheck :: (Ord a, Show a, Num a, Fractional a) => a -> a -> a -> a -> String
fuelCheck d g s m = 
    let distanceToEmpty = g - m
        timeToArrival   = show $ (d / s) * 60
        timeToEmpty     = show $ (distanceToEmpty / s) * 60
    in 
        if d > distanceToEmpty   
            then "You have enough fuel and you will arive in " ++ timeToArrival ++ " minutes."
            else
                if d < distanceToEmpty   
                    then "You need to refule in " ++ timeToEmpty ++ " minutes or you will run out of gas."
                    else "You might make it there"

-- Question 4
-- Write a function that takes in two numbers and returns their quotient such that it is not greater than 1.
-- Return the number as a string, and in case the divisor is 0, return a message why the division is not
-- possible. To implement this function using both guards and if-then-else statements.  


-- Question 5
-- Write a function that takes in two numbers and calculates the sum of squares for the product and quotient
-- of those numbers. Write the function such that you use a where block inside a let expression and a
-- let expression inside a where block.
