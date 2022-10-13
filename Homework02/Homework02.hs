
-- Question 1
-- Add the type signatures for the functions below and then remove the comments and try to compile.
-- (Use the types presented in the lecture.)

f1 :: Floating a => a -> a -> a -> a
f1 x y z = x ** (y/z)

f2 :: Floating a => a -> a -> a -> a
f2 x y z = sqrt (x/y - z)

f3 :: Bool -> Bool -> [Bool]
f3 x y = [x == True] ++ [y]

f4 :: Eq a => [a] -> [a] -> [a] -> Bool
f4 x y z = x == (y ++ z)


-- Question 2
-- Are really all variables in Haskell immutable? Try googling for the answer.
{-
Variables within let and where statements are not global. They can be reused in different let and where statements. Variables
can be re defined in GHCi but it cannot be rediefined when compiling a .hs file.
-}

-- Question 3
-- Why should we define type signatures of functions? How can they help you? How can they help others?
{-
They help remind us what the function is supposed to be used for. It can limit errors because the function won't accept 
data types that it isn't designed for.
-}

-- Question 4
-- Why should you define type signatures for variables? How can they help you?
{-
It constrains the type of data that can be asigned to the variable. This could lead to less errors in the program.
-}

-- Question 5
-- Are there any functions in Haskell that let you transform one type to the other? Try googling for the answer.
{-
show - converts a Num type to a String type
read - converts a String type to a Num type
-}

-- Question 6
-- Can you also define in Haskell list of lists? Did we showed any example of that? How would you access the inner
-- most elements?
{-
Yes, ther are lists of lists. You can access the inner lists multiple different ways depending on what your end goal is.
One could use pattern matching, currying, filtering, etc.
-}