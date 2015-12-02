{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
module TestHelpers where

import Data.List hiding (sortOn)
import Data.Word
import System.Random
import Control.Applicative
import Control.Monad

import safe GameExercise
    (Player(..), Board, Move, Tree(..), LRand(..), Seed, evalRand, getRandom, play, allowedMoves, treeOf, computerFirst, computerSecond, randomFirst, randomSecond, computerFirstHeuristic, computerSecondHeuristic, toBoard, fromBoard, toMove, fromMove)

-- Convention: (width, height)
type BoardSize = (Int, Int)
-- Convention: (x, y)
-- Convention: 0 <= x < width
--             0 <= y < height
type Position  = (Int, Int)

data Board2 = B2
    { bSize   :: BoardSize
    , bPos    :: [Position]
    , bPlayer :: Player -- player who is next to go
    } deriving (Ord)

instance Eq Board2 where
    (B2 s pos p) == (B2 s' pos' p') =
        s == s' && p == p' && (sort (nub pos)) == (sort (nub pos'))

data Move2 = M2
    { mPlayer :: Player
    , mPos    :: Position
    } deriving (Eq, Ord)

data Tree2 = Fork2
    { root2 :: Board2
    , children2 :: [(Move2,Tree2)]
    } deriving (Show, Eq)

-- Be a lot less verbose
instance Show Move2 where
  showsPrec prec (M2 p pos)
    = showParen (prec > 0) $ showString $ "M2 " ++ show p ++ " " ++ show pos

instance Show Board2 where
  showsPrec prec (B2 siz pos pl)
    = showParen (prec > 0) $ showString $ "B2 " ++ show siz ++ " " ++ show pos ++ " " ++ show pl


toBoard2 :: (Int, Int, [Position], Player) -> Board2
toBoard2 (sX, sY, pos, pl) = B2 (sX, sY) pos pl

fromBoard2 :: Board2 -> (Int, Int, [Position], Player)
fromBoard2 (B2 (sX, sY) pos pl) = (sX, sY, pos, pl)

toMove2 :: Board2 -> (Int, Int, Player) -> Move2
toMove2 _ (x,y,pl) = M2 pl (x,y)

fromMove2 :: Move2 -> (Int, Int, Player)
fromMove2 (M2 pl (x,y)) = (x,y,pl)


mTm2 :: Board2 -> Move -> Move2
mTm2 b = toMove2 b . fromMove

m2Tm :: Board -> Move2 -> Move
m2Tm b = toMove b . fromMove2

msTms2 :: Board2 -> [Move] -> [Move2]
msTms2 _ [] = []
msTms2 brd (m:ms) = mTm2 brd m : msTms2 brd' ms
    where
        brd' = bTb2 $ play m $ b2Tb brd

bTb2 :: Board -> Board2
bTb2 = toBoard2 . fromBoard

b2Tb :: Board2 -> Board
b2Tb = toBoard . fromBoard2

tTt2 :: Tree -> Tree2
tTt2 (Fork r ch) = Fork2 (bTb2 r) [(mTm2 (bTb2 r) m, tTt2 t) | (m, t) <- ch]

t2Tt :: Tree2 -> Tree
t2Tt (Fork2 r ch) = Fork (b2Tb r) [(m2Tm (b2Tb r) m, t2Tt t) | (m, t) <- ch]

testMove :: (Move2, Board2) -> Move2
testMove (m2,b2) = mTm2 b2 . toMove b . fromMove $ m2Tm b m2
    where
        b = b2Tb b2

testMove' :: ((Int, Int, Player), Board2) -> (Int, Int, Player)
testMove' (m2,b2) = fromMove . toMove b $ m2
    where
        b = b2Tb b2

testPlay :: (Move2, Board2) -> Board2
testPlay (m, b) = bTb2 $ play (m2Tm b' m) b'
    where
        b' = b2Tb b


checkMove :: [(Move2,Tree2)] -> Move2 -> Maybe Tree2
checkMove [] m = Nothing
checkMove ((m1,t1):fs) m = if m == m1 then Just t1 else checkMove fs m

--          Start    player1    player2    final board
playGame :: Tree2 -> [Move2] -> [Move2] -> Board2
playGame t [] _ = root2 t
playGame (Fork2 b2 ff) (m:ms) opponent = case checkMove ff m of
                    Nothing -> error $ "An illegal move has been made, and not caught somewhere more sensible."
                    Just t  -> playGame t opponent ms


testAllowedMoves :: Board2 -> [Move2]
testAllowedMoves b2 = sort
                    . map (mTm2 b2)
                    . allowedMoves
                    $ b2Tb b2


---------------------------
-- testing deterministic --
-- solutions             --
---------------------------
notImplementedYet :: Bool
notImplementedYet = error "This test is not implemented yet"

testComputers :: (Board2, Player) -> ()
testComputers (b, pl) | goForIt t = ()
                      | otherwise = error $ plName ++ " should have won on the board " ++ show b
    where
        t = tTt2 $ treeOf $ b2Tb b

        (goForIt,plName) | pl == PH  = (testComputerFirst,  "computerFirst")
                         | otherwise = (testComputerSecond, "computerSecond")


testComputerFirst :: Tree2 -> Bool
testComputerFirst t = checkStrategy t computer player2
    where computer = getPlayerMoves1 t player2
          player2 = magicTest2 t computer

testComputerSecond :: Tree2 -> Bool
testComputerSecond t = checkStrategy t player1 computer
    where computer = getPlayerMoves2 t player1
          player1 = magicTest2 t computer

checkStrategy :: Tree2 -> [Move2] -> [Move2] -> Bool
checkStrategy t2 [] _ = False

checkStrategy t2 (m:_) [] = case checkMove (children2 t2) m of
                Nothing -> error $
                    "Player 1 has made an illegal move which should have been spotted earlier in the Test. Please report this as a bug."
                Just _  -> True
checkStrategy t2 (m:ms) (opp:oppMoves) =
    let subtree' = case checkMove (children2 t2) m of
                Nothing -> error $
                    "Player 1 has made an illegal move which should have been spotted earlier in the Test. Please report this as a bug."
                Just t  -> t
        subtree  = case checkMove (children2 subtree') opp of
                Nothing -> error $
                    "Player 2 has made an illegal move which should have been spotted earlier in the Test. Please report this as a bug."
                Just t  -> t
    in checkStrategy subtree ms oppMoves

getPlayerMoves1 :: Tree2 -> [Move2] -> [Move2]
getPlayerMoves1 tree m2s =
   let playerMoves = rewritePlayerMoves tree m2s $ computerFirst (t2Tt tree) oppMoves
       oppMoves = rewriteOppMoves tree m2s playerMoves
    in playerMoves

getPlayerMoves2 :: Tree2 -> [Move2] -> [Move2]
getPlayerMoves2 t [] = map (mTm2 (root2 t)) $ computerSecond (t2Tt t) []
getPlayerMoves2 t@(Fork2 board tt) mm@(m2:m2s) = 
    let m = m2Tm (b2Tb board) m2
        t' = case checkMove tt m2 of
               Nothing -> error $ "We've made an illegal move while checking computerSecond. Please report this as a bug."
               Just t2 -> t2
        oppMoves = case playerMoves of
                     [] -> []
                     (m':m's) -> let t'' = case checkMove (children2 t') m' of
                                            Nothing -> error $ "You've made an illegal move"
                                            Just x -> x
                                     in rewriteOppMoves t'' m2s playerMoves
        playerMoves = rewritePlayerMoves t' m2s $ computerSecond (t2Tt t) (m:oppMoves)
        t'' = undefined
     in playerMoves

rewritePlayerMoves :: Tree2 -> [Move2] -> [Move] -> [Move2]
rewritePlayerMoves _ _ [] = []
rewritePlayerMoves t2 [] (m:ms) = case ms of
        [] -> [m2]
        _  -> error $ "You've continued to play after the final move " ++ (show m2)
                ++ " on board "++ (show board)
    where
        board = root2 t2
        m2 = mTm2 board m
rewritePlayerMoves t2@(Fork2 board2 ff) (oppMove:responses) (m : ms) =
    let m2 = mTm2 board2 m
        subtree' = case checkMove ff m2 of
                Nothing -> error $ "You've made an illegal move: " ++ (show m2) ++ 
                    " is not a valid move in " ++ (show board2)
                Just t2 -> t2
        subtree = case checkMove (children2 subtree') oppMove of
                Nothing -> error $ 
                    "We've made an illegal move while testing: " ++
                    (show m2) ++  " is not a valid move in " ++ (show board2) ++
                    ". Please report this as a bug."
                Just t2 -> t2
     in m2 : rewritePlayerMoves subtree responses ms


--                 state    opponent   player
rewriteOppMoves :: Tree2 -> [Move2] -> [Move2] -> [Move]
rewriteOppMoves _ [] _ = []
rewriteOppMoves t2@(Fork2 board []) os@(oppMove:responses) [] = 
     error $
         "It seems we've tried to keep playing after you lost on board " ++ (show board) ++
         ". Please report this as a bug."
rewriteOppMoves t2@(Fork2 board []) os@(oppMove:responses) (m:ms) = 
     error $ "You've tried to play when there were no legal moves on board " ++ (show board)
rewriteOppMoves t2@(Fork2 board ff) os@(oppMove:responses) (m:ms) = 
    let oppMove' = m2Tm (b2Tb board) oppMove
        subtree' = case checkMove ff m of
            Nothing ->  error $ "You've made an illegal move: " ++ (show m) ++ 
                " is not a valid move in " ++ (show board)
            Just t2 -> t2
        subtree = case checkMove (children2 subtree') oppMove of
            Nothing -> error $ "We've made an illegal move while testing: " ++
                (show m) ++  " is not a valid move in " ++ (show board) ++
                    ". Please report this as a bug."
            Just t2 -> t2
     in oppMove' : (rewriteOppMoves subtree responses ms)


----------------
-- Some magic --
----------------

--this number is liable to change drastically.
magicNumber = 104729
doMagic = toss magicNumber
    where
    toss = flip flip 0 . flip magicRing
    magicRing d r = if d > r then (\ n -> r) else rock magicRing (r-) d
    rock a d c = a c (d c)

magicTest1 (Fork2 _ []) _ = []
magicTest1 (Fork2 _ ts) [] = [ fst $ ts !! doMagic (length ts) ]
magicTest1 (Fork2 _ ts) (m:ms) =
            let n = doMagic (length ts)
                (move,(Fork2 b' tt)) = ts !! n
                t' = case checkMove tt m of
                      Nothing -> error $ "You have made an illegal move: " ++
                          (show m) ++ " is not a legal move on " ++ (show b')
                      Just t2 -> t2
             in move : magicTest1 t' ms

magicTest2 t2 [] = []
magicTest2 (Fork2 board ts) (m:ms) =
              let t = case checkMove ts m of
                      Nothing -> error $ "You have made an illegal move: " ++
                          (show m) ++ " is not a legal move on " ++ (show board)
                      Just t2 -> t2
               in magicTest1 t ms

---------------------
---------------------

-- Does two things:
--
-- - Sorts the branches of a tree, so that the "same" trees can compare equal (up to a certain depth)
-- - Checks that the same move does not appear twice, and returns an error otherwise.
--
-- Is still lazy.

normCheckTree2 (Fork2 r ch)
    = check `seq` Fork2 r normSortedChildren
  where
    check = if moves == fastNub moves then () else error $ "Duplicate moves in list: " ++ show moves
    moves = map fst normSortedChildren

    normalisedChildren = map (\(move, t) -> (move, normCheckTree2 t)) ch
    normSortedChildren = TestHelpers.sortOn fst normalisedChildren


-- for GHC 7.6.3
sortOn :: (Ord b) => (a -> b) -> [a] -> [a]
sortOn f xs = sortBy ordering xs
  where ordering x y = f x `compare` f y

-- We want to compare trees, but this ain't easy.
--
-- We compare them in a randomised fashion:
--
-- 1. Pick a seed.
-- 2. Take two trees, t1 and t2. They should both have been normalised using normCheckTree2.
-- 3. The two root boards should be equal as Board2's.
-- 4. Look at the list of all edge labels (moves) from t1, and from t2.
--
--    These lists should be equal. If not, the check fails.
--
-- 5. If the lists are nonempty, pick a random edge and continue at 3.
--
-- This will terminate, because the trees are well-founded. It will also
-- generate a trace. The following function will give you the trace, starting from step 4.
--
-- The format is as follows: a list of:
--
-- a. The list of moves at the top level
-- b. The move chosen
-- c. The board that we arrived at
--
-- In type FullTrace, we also list the root board and the root list of moves.

type TraceRest = [([Move2], Move2, Board2)]
type FullTrace = (Board2, TraceRest)
data Seed2 = Seed2 { unSeed2 :: Word32 }

instance Show Seed2 where
    show (Seed2 x) = "Seed2 " ++ show x

-- Input tree must be normalised.
restTraceFrom2 :: StdGen -> [(Move2, Tree2)] -> TraceRest
restTraceFrom2 _ [] = []
restTraceFrom2 gen xs
    = (map fst xs, move, board) : restTraceFrom2 gen' ch
  where
    (n, gen') = randomR (0, length xs - 1) gen
    (move, subtree@(Fork2 board ch)) = xs !! n

-- Input tree must (again) be normalised.
randomTraceFromTree2 :: Seed2 -> Tree2 -> FullTrace
randomTraceFromTree2 (Seed2 seed) (Fork2 b ch) = (b, restTraceFrom2 gen ch)
  where
    gen = mkStdGen $ fromIntegral seed

randomTraceFromBoard :: (Seed2, Board2) -> FullTrace
randomTraceFromBoard (seed, b2) = randomTraceFromTree2 seed (normCheckTree2 $ tTt2 $ treeOf b)
  where
    b = b2Tb b2

--- Testing randomness
----------------------
enoughRandomMoves :: (Board2, Int) -> Bool
enoughRandomMoves (brd2, seed)
        | not allMovesCorrect = error $ "not all generated moves for the board " ++ show brd2 ++ " were correct"
        | not (numWithoutDupl * 2 > numOfTests)
            = error ("not enough unique plays: only "
                    ++ show numWithoutDupl
                    ++ " unique plays out of "
                    ++ show numOfTests
                    ++ " for the board " ++ show brd2
                    ++ ".")

        | otherwise = True
    where
        numOfTests = 20
        brd1     = b2Tb brd2
        gameTree = treeOf brd1

        trees   = replicate numOfTests gameTree
        seeds   = evalRand (replicateM numOfTests randInt) $ mkStdGen seed

        toComputeWith = zipWith (,) trees seeds

        randFun :: (Tree, Int) -> [Move]
        randFun (t, seed') = rplay seed' (randomFirst t) (randomSecond t)
        -- randFun (t, seed') = iplay (computerFirst t) (computerSecond t)

        generatedMoves = map (msTms2 brd2)
                       $ map randFun toComputeWith

        withoutDuplicates = fastNub generatedMoves
        numWithoutDupl    = length withoutDuplicates

        allMovesCorrect = all checkOnePlay withoutDuplicates

        checkOnePlay :: [Move2] -> Bool
        checkOnePlay moves =
            playGame (tTt2 gameTree) (odds moves) (evens moves) `seq` True

uniq :: Eq a => [a] -> [a]
uniq (a1:a2:xs)
    | a1 == a2 = uniq (a2:xs)
    | otherwise = a1 : (uniq (a2:xs))
uniq [a1] = [a1]
uniq [] = []

fastNub :: (Ord a, Eq a) => [a] -> [a]
fastNub = uniq . sort


randInt :: LRand Int
randInt = getRandom


rplay :: Int -> ([a] -> LRand [a]) -> ([a] -> LRand [a]) -> [a]
rplay gen f g = iplay (\xs -> evalRand (f xs) seed) (\xs -> evalRand (g xs) seed)
    where
        seed = mkStdGen gen

iplay :: ([a]->[a]) -> ([a]->[a]) -> [a]
iplay f g = intercal ys xs
    where
        ys = f xs
        xs = g ys

intercal :: [a] -> [a] -> [a]
intercal []     ys = ys
intercal (x:xs) ys = x : intercal ys xs

evens :: [a] -> [a]
evens [] = []
evens (x:xs) = odds xs

odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x:evens xs


whoSMove (M2 pl _) = pl

