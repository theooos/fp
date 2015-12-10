{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module GameExercise where

{-

Before running your solution in ghci or compiling it by ghc on lab machines
make sure you run

    module load ghc/7.6.3

(This is to make sure your version of GHC supports Safe language extension.)

-}

import Control.Monad (liftM, ap)
import Control.Applicative (Applicative(..))
import System.Random

-- We have two players PV (the vertical player) and PH (the horizontal player).
-- More information is in the pdf handout on Canvas.

data Player = PV | PH deriving (Read, Show, Eq, Ord)

-- You need to define a type of boards:

data Board -- = <complete a suitable definition provided by you>

-- You also need to provide a type of moves:

data Move -- = <complete a suitable definition provided by you>

-- You will work with the definition of Tree as in our tic-tac-toe
-- lectures and handouts:

data Tree = Fork {root :: Board, children :: [(Move,Tree)]}

-- In order to test your program, we will need to create boards,
-- without knowing how you choose to represent boards. You have to
-- implement the following function toBoard for that purpose.
--
-- Input: a tuple (xBound, yBound, coordinates, player) where
--
--     (1) xBound and yBound are non-negative Int's
--     (2) coordinates is a list of pairs (x,y) with 0 <= x < xBound
--                                               and 0 <= y < yBound
--         which are to be occupied in the board.
--     (3) player is the Player that plays next in that board.
--
-- Output: a Board according to your choice of representation.

toBoard :: (Int, Int, [(Int,Int)], Player) -> Board
toBoard = undefined

-- We also need to perform the opposite conversion, from your
-- representation of boards, to our naive description of boards, so
-- that we can "read" your boards whenever we need to, for testing
-- purposes:

fromBoard :: Board -> (Int, Int, [(Int, Int)], Player)
fromBoard = undefined

-- Similarly, given a Board, we want to create a Move given
-- (x,y,player) where (x,y) is a position in the Board:

toMove :: Board -> (Int, Int, Player) -> Move
toMove = undefined

-- And we want to do the opposite too:

fromMove :: Move -> (Int, Int, Player)
fromMove = undefined

fromMove' :: Board -> Move -> (Int, Int, Player)
fromMove' b = fromMove

-- The first exercise is to play an allowed move in a board, and get
-- the resulting board. Simply throw an error if the move is not
-- allowed in that board. We will only test your function with allowed
-- moves:

play :: Move -> Board -> Board
play = undefined

-- Ah. But what are the allowed moves in a give board? You tell me:

allowedMoves :: Board -> [Move]
allowedMoves = undefined

-- Now build the tree of a game. You are allowed to base your
-- implementation on any of the given treeOf implementations for the
-- several tic-tac-toe programs in Canvas (discussed in the lectures):

treeOf :: Board -> Tree
treeOf = undefined

-- only serves for the example.
domineering :: Int -> Int -> Tree
domineering x y = undefined -- only serves for the example.

-- Now we want to have the computer playing first, lazily against an
-- opponent. The opponent supplies the list of moves. But the computer
-- is not allowed to cheat. It has to play its first move without
-- looking at any of the moves of the opponent:

computerFirst :: Tree -> [Move] -> [Move]
computerFirst  = undefined

-- And now you want the computer to play second. It will have to first
-- check the head move of the opponent, provided the list of moves is
-- non-empty, and base its first move (the head of the output list) on
-- that:

computerSecond :: Tree -> [Move] -> [Move]
computerSecond = undefined

-- This should be done so that the following example works:

iplay :: ([Move]->[Move]) -> ([Move]->[Move]) -> [Move]
iplay f g = intercal ys xs
  where
    ys = f xs
    xs = g ys

intercal :: [a] -> [a] -> [a]
intercal []     ys = ys 
intercal (x:xs) ys = x : intercal ys xs

-- What the following example should do is produce the list of moves
-- that results from having the computer playing against itself:

example :: Int -> Int -> [Move]
example x y = iplay (computerFirst (domineering x y)) (computerSecond (domineering x y))


-- We now move to random playing. The randomness monad we used for
-- quick sort in the lecture is not sufficiently lazy for our
-- purposes. We work with a lazy Random monad based on
--
--   https://hackage.haskell.org/package/MonadRandomLazy-0.1/docs/Control-Monad-LazyRandom.html
--
-- instead, define below.  


-- We use the standard random generator as our type of seeds for
-- random things:

type Seed = StdGen

-- We get seeds for random-thing generation from Int's:

mkSeed :: Int -> Seed
mkSeed = mkStdGen

-- See https://en.wikipedia.org/wiki/Random_seed
-- We define the monad as follows:

newtype LRand a = LRand (Seed -> a)

instance Functor LRand where
 fmap f (LRand h) = LRand (f.h)

instance Applicative LRand where
 pure  = return
 (<*>) = ap

instance Monad LRand where
 return x = LRand (\seed -> x)  -- The seed is ignored.

 LRand m >>= k =                -- The seed is not only used, but also transformed and propagated.
   LRand (\s ->
     let (s1,s2)  = split s     -- The split function is predefined in the random libraries. Hoogle it.
         LRand m' = k (m s1)
      in m' s2
   )

-- The following are to "get out" this monad:

evalRand :: LRand a -> Seed -> a
evalRand (LRand f) s = f s

-- What this says is that if you have a random element of type a (that
-- is, something of type LRand a), one way to get something of type a
-- is to provide a seed.

-- This is like the above, but also produces a new seed, if we need it:

runRand :: LRand a -> Seed -> (a, Seed)
runRand (LRand f) s = (f s1, s2)
 where (s1, s2) = split s

-- And finally we need to be able to generate random elements:

getRandom :: Random a => LRand a
getRandom = LRand $ fst . random

-- But this needs a to be in the Random type class. Most types are
-- automatically there, and it is unlikely you will need to worry
-- about this in this exercise, unless you do very sophisticated
-- things.

-- We also may need to get random elements within a range:

getRandomR :: Random a => (a,a) -> LRand a
getRandomR range = LRand $ fst . randomR range

-- This is the end of our definition of our lazy randomness monad.

randomFirst :: Tree -> [Move] -> LRand [Move]
randomFirst = undefined

randomSecond :: Tree -> [Move] -> LRand [Move]
randomSecond = undefined

computerFirstHeuristic :: Board -> [Move] -> [Move]
computerFirstHeuristic = undefined 

computerSecondHeuristic :: Board -> [Move] -> [Move]
computerSecondHeuristic = undefined

