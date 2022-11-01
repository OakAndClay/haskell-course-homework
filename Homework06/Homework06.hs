-- The first batch of functions here will be exercises from Learn You a Haskell (LYH) and the lesson.
-- I am trying to write these without looking at the solution.

-- My first attempt at replicate' mistakenly returned the value into the list instead of an empty list. When r = 0
-- This produced a returned list that was 1 element too long.
{-
replicate' :: Int -> a -> [a]
replicate' 0 v = [v]
replicate' r v = v : replicate' (r-1) v
-}
-- The correct implementation is the answer to Q2

-- My first attempt at take' forgot to account for the possibility of a negative imput for r.
{-
take' :: Int -> [a] -> [a]
take' 0 _      = []
take' r []     = []
take' r (x:xs) = x : take' (r-1) xs
-}

-- This is a correct implementation
take' :: Int -> [a] -> [a]
take' r _ 
  | r <= 0     = []
take' r []     = []
take' r (x:xs) = x : take' (r-1) xs

-- My first implementation of zip' returned a list combining the two different lists
{-
zip' :: [a] -> [a] -> [a]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = [x,y] ++ zip' xs ys
-}

-- zip' in LYH is suposed to return a list of tuples so that two different types could be combined. I'll do that with zip''
{-
zip'' :: [a] -> [b] -> [(a,b)]
zip'' _ [] = []
zip'' [] _ = []
zip'' (x:xs) (y:ys) = (x,y):zip'' xs ys
-}


-- Question 1
-- Write a function called `repeat'` that takes a value and creates an infinite list with
-- the value provided as every element of the list.
--
-- >>> repeat 17
--[17,17,17,17,17,17,17,17,17...
repeat' :: a -> [a]
repeat' a = a : repeat' a

-- Question 2
-- Using the `repeat'` function and the `take` function we defined in the lesson (comes with Haskell),
-- create a function called `replicate'` that takes a number `n` and a value `x` and creates a list
-- of length `n` with `x` as the value of every element. (`n` has to be Integer.)
--
-- This is implementation produces correct results.
replicate'' :: Int -> a -> [a]
replicate'' r v
  | r <= 0 = []
  | otherwise = v : replicate'' (r-1) v

-- >>> replicate 0 True
-- []
-- >>> replicate (-1) True
-- []
-- >>> replicate 4 True
-- [True,True,True,True]




-- Question 3
-- Write a function called `concat'` that concatenates a list of lists.
--
concat' :: [[a]] -> [a]
concat' []   = []
concat' [x]    = x
concat' (x:xs) = x ++ concat' xs

-- >>> concat' [[1,2],[3],[4,5,6]]
-- [1,2,3,4,5,6]


-- Question 4
-- Write a function called `zip'` that takes two lists and returns a list of
-- corresponding pairs (zips them) like this:
--

zip' :: [a] -> [b] -> [(a,b)]
zip' _ []          = []
zip' [] _          = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- >>> zip' [1, 2] ['a', 'b']
-- [(1,'a'),(2,'b')]
--
-- If one input list is shorter than the other, excess elements of the longer
-- list are discarded, even if one of the lists is infinite:
--
-- >>> zip' [1] ['a', 'b']
-- [(1,'a')]
-- >>> zip' [1, 2] ['a']
-- [(1,'a')]
-- >>> zip' [] [1..]
-- []
-- >>> zip' [1..] []
-- []



-- Question 5
-- Create a function called `zipWith'` that generalises `zip'` by zipping with a
-- function given as the first argument, instead of a tupling function.
--
-- > zipWith' (,) xs ys == zip' xs ys
-- > zipWith' f [x1,x2,x3..] [y1,y2,y3..] == [f x1 y1, f x2 y2, f x3 y3..]
--
-- For example, `zipWith' (+)` is applied to two lists to produce the list of
-- corresponding sums:
--
-- >>> zipWith (+) [1, 2, 3] [4, 5, 6]
-- [5,7,9]
zipwith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith' _ _ []  = []
zipwith' _ [] _  = []
zipwith' f (x:xs) (y:ys) = f x y : zipwith' f xs ys


-- Question 6
-- Write a function called `takeWhile'` that takes a predicate and a list and
-- returns the list up until an element that doesn't satisfy the predicate.
--
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) 
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- >>> takeWhile (< 3) [1,2,3,4,1,2,3,4]
-- [1,2]
-- >>> takeWhile (< 9) [1,2,3]
-- [1,2,3]
-- >>> takeWhile (< 0) [1,2,3]
-- []


-- Question 7 (More difficult)
-- Write a function that takes in an integer n, calculates the factorial n! and
-- returns a string in the form of 1*2* ... *n = n! where n! is the actual result.
factorial :: (Show a, Num a, Enum a) => a -> [Char]
factorial n = init (insertMult [1..n]) ++ " = " ++ show (product [1..n])
  where
    insertMult [] = ""
    insertMult (x:xs) = show x ++ "*" ++ insertMult xs
    product [] = 1
    product (x:xs) = x * product xs
-- Question 8
-- Below you have defined some beer prices in bevogBeerPrices and your order list in
-- orderList + the deliveryCost. Write a function that takes in an order and calculates
-- the cost including delivery. Assume that the two lists have the beers in the same order.



bevogBeerPrices :: [(String, Double)]
bevogBeerPrices =
  [ ("Tak", 6.00),
    ("Kramah", 7.00),
    ("Ond", 8.50),
    ("Baja", 7.50)
  ]

orderList :: [(String, Double)]
orderList =
  [ ("Tak", 5),
    ("Kramah", 4),
    ("Ond", 7)
  ]

deliveryCost :: Double
deliveryCost = 8.50

order :: [(String, Double)] -> [(String, Double)] -> Double
order [] _ = deliveryCost
order _ [] = deliveryCost
order ((_,x):xs) ((_,y):ys) = (x*y) + order xs ys