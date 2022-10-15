import Distribution.Simple.Utils (xargs)
-- Question 1
-- Lets say you have the nested values defined bellow. How would you get the value of
-- 4 by using only pattern matching in a function?

nested :: [([Int], [Int])]
nested = [([1,2],[3,4]), ([5,6],[7,8])]

get4 :: (Eq a1, Eq a2, Num a1, Num a2) => [([a1], [a2])] -> String
get4 [([4,_],[_,_]),([_,_],[_,_])] = "4 is in the first position" 
get4 [([_,4],[_,_]),([_,_],[_,_])] = "4 is in the second position" 
get4 [([_,_],[4,_]),([_,_],[_,_])] = "4 is in the third position" 
get4 [([_,_],[_,4]),([_,_],[_,_])] = "4 is in the fourth position" 
get4 [([_,_],[_,_]),([4,_],[_,_])] = "4 is in the firth position" 
get4 [([_,_],[_,_]),([_,4],[_,_])] = "4 is in the sixth position" 
get4 [([_,_],[_,_]),([_,_],[4,_])] = "4 is in the seventh position" 
get4 [([_,_],[_,_]),([_,_],[_,4])] = "4 is in the eigth position" 
get4 [([_,_],[_,_]),([_,_],[_,_])] = "There is no 4" 

get4' :: Show a1 => [([a2], [a1])] -> [Char]
get4'[([_,_],[_,x]),([_,_],[_,_])] = "The fourth position is a " ++ show x ++ "."
get4' _                            = "It doesn't matter."                            

-- Question 2
-- Write a function that takes a list of elements of any type and, if the list has 3 or more elements, it
-- removes them. Else, it does nothing. Do it two times, one with multiple function definitions and one with
-- case expressions.

-- Guards version
three :: [a] -> [a]
three xs
    | length xs >= 3 = []
    | otherwise      = xs

-- Case of
three' :: [a] -> [a]
three' xs = case xs of
    (_:_:_:xs) -> []
    x          -> x 

-- Pattern matching functions
three'' :: [a] -> [a]
three'' (_:_:_:xs) = xs
three'' x          = x       

-- Question 3
-- Create a function that takes a 3-element tuple (all of type Integer) and adds them together
sumTuple3 :: (Integer, Integer, Integer) -> Integer
sumTuple3 (x,y,z) = x + y + z

-- Question 4
-- Implement a function that returns True if a list is empty and False otherwise.
emptyList :: [a] -> Bool
emptyList [] = True
emptyList _  = False

-- Question 5
-- Write the implementation of the tail function using pattern matching. But, instead of failing if
-- the list is empty, return an empty list.
tail' :: [a] -> [a]
tail' []      = []
tail' (x:xs)  = xs

-- Question 6
-- write a case expression wrapped in a function that takes an Int and adds one if it's even. Otherwise does nothing. 
-- (Use the `even` function to check if the number is even.)

