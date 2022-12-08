-- import Main (Move(GoRight))
-- import Data.Time.Format.ISO8601 (yearFormat)
{-

**************************** IMPORTANT ****************************

Solve this homework after completing and checking the "Maze" one.

*******************************************************************

We're going to build on top of the "Maze" challenge by coding a similar
but a bit more complicated game.

It works the same as the "Maze" game, with the difference that the player
is now in a forest. Because we're in a forest, there are no walls. And,
if you walk long enough, you're guaranteed to find the exit.

So, what's the challenge in playing this game? The challenge lies in that
now we have "stamina." Stamina is a number (we start with 10). And, each
time the player makes a move, its stamina gets reduced by the amount of work
needed to cross the current trail (represented by a number contained in the
value constructor).

The data types and functions are pretty much the same, with a few caveats:

- We don't have walls.
- We don't want to choose a specific numeric type, but we want to make sure
we can do basic numeric operations regardless of the type we pass to the functions.
- Because now we have to keep track of the player's stamina, we'll need to
move it around with our current forest. This would be an awesome use case
for monads, but because we don't know how to use them yet, a "(stamina, forest)"
pair will have to do.

Using GHCi, like the "Maze" game, this game should look like this:

*Main> solveForest testForest []
"You have 10 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward ]
"You have 7 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward]
"You have 4 stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."
*Main> solveForest testForest [GoForward, GoForward, GoLeft  ]
"You ran out of stamina and died -.-!"
*Main> solveForest testForest [GoForward, GoLeft , GoRight]
"YOU'VE FOUND THE EXIT!!"
-}

data Move     = GoLeft | GoForward | GoRight
data Forest a = FoundExit | Trail a (Forest a) (Forest a) (Forest a) deriving (Show, Eq)
type Hike     = (Int, Forest Int)

move :: Forest Int -> Move -> Forest Int
move FoundExit _                = FoundExit
move (Trail _ y _ _) GoLeft     = y
move (Trail _ _ y _) GoForward  = y
move (Trail _ _ _ y) GoRight    = y

stamina :: Hike -> Int
stamina (a,Trail x _ _ _) = a - x

hike :: Hike -> Move -> Hike
hike (x,y) z = (stamina (x,y), move y z)

foldForest :: Hike -> [Move] -> Hike
foldForest = foldl hike

showCurrentChoice :: Hike -> String
showCurrentChoice (x,y)
    | x <= 0         = "You ran out of stamina and died -.-!"
    | y == FoundExit = "YOU'VE FOUND THE EXIT!!"
    | otherwise      = "You have " ++ show x ++ " stamina, and you're still inside the Forest. Choose a path, brave adventurer: GoLeft, GoRight, or GoForward."

solveForest :: Forest Int -> [Move] -> String
solveForest y z = showCurrentChoice $ foldForest (10,y) z

testForest :: Forest Int
testForest =
  Trail
    3
    ( Trail
        7
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 4 FoundExit FoundExit FoundExit)
        (Trail 5 FoundExit FoundExit FoundExit)
    )
    ( Trail
        3
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 9 FoundExit FoundExit FoundExit)
        (Trail 5 FoundExit FoundExit FoundExit)
    )
    ( Trail
        5
        (Trail 3 FoundExit FoundExit FoundExit)
        (Trail 4 FoundExit FoundExit FoundExit)
        (Trail 1 FoundExit FoundExit FoundExit)
    )