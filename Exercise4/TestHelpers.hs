{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE BangPatterns #-}
module TestHelpers where

import Data.List hiding (sortOn)
import Data.Word
import System.Random
import Control.Applicative
import Control.Monad
import Control.Exception (assert, catch, SomeException)
import Test.QuickCheck.Exception (tryEvaluateIO, AnException(..))
import System.Timeout
import Data.Time.Clock
import System.Mem

import safe GameExercise
    (Player(..), Board, Move, Tree(..), LRand(..), Seed, evalRand, getRandom, play, allowedMoves, treeOf, computerFirst, computerSecond, randomFirst, randomSecond, computerFirstHeuristic, computerSecondHeuristic, toBoard, fromBoard, toMove, fromMove')
import qualified TestHelpersSecret as S

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

toMove2 :: (Int, Int, Player) -> Move2
toMove2 (x,y,pl) = M2 pl (x,y)

fromMove2 :: Move2 -> (Int, Int, Player)
fromMove2 (M2 pl (x,y)) = (x,y,pl)


sortMoveList :: (Int, Int, [(Int, Int)], Player) -> (Int, Int, [(Int, Int)], Player)
sortMoveList (a, b, l, d) = (a, b, sort l, d)

mTm2 :: Board -> Move -> Move2
mTm2 b = toMove2 . fromMove' b

m2Tm :: Board -> Move2 -> Move
m2Tm b = toMove b . fromMove2

msTms2 :: Board2 -> [Move] -> [Move2]
msTms2 _ [] = []
msTms2 brd (m:ms) = mTm2 (b2Tb brd) m : msTms2 brd' ms
    where
        brd' = bTb2 $ play m $ b2Tb brd

ms2Tms :: Board -> [Move2] -> [Move]
ms2Tms _ [] = []
ms2Tms brd (m2:m2s) = m : ms2Tms brd' m2s
    where
        m = m2Tm brd m2
        brd' = play m brd

bTb2 :: Board -> Board2
bTb2 = toBoard2 . fromBoard

b2Tb :: Board2 -> Board
b2Tb = toBoard . fromBoard2

tTt2 :: Tree -> Tree2
tTt2 (Fork r ch) = Fork2 (bTb2 r) [(mTm2 r m, tTt2 t) | (m, t) <- ch]

t2Tt :: Tree2 -> Tree
t2Tt (Fork2 r ch) = Fork (b2Tb r) [(m2Tm (b2Tb r) m, t2Tt t) | (m, t) <- ch]

testMove :: (Move2, Board2) -> Move2
testMove (m2,b2) = mTm2 b . toMove b . fromMove' b $ m2Tm b m2
    where
        b = b2Tb b2

testMove' :: ((Int, Int, Player), Board2) -> (Int, Int, Player)
testMove' (m2,b2) = fromMove' b . toMove b $ m2
    where
        b = b2Tb b2

testPlay :: (Move2, Board2) -> Board2
testPlay (m, b) = bTb2 $ play (m2Tm b' m) b'
    where
        b' = b2Tb b


checkMove :: [(Move2,Tree2)] -> Move2 -> Maybe Tree2
checkMove [] m = Nothing
checkMove ((m1,t1):fs) m = if m == m1 then Just t1 else checkMove fs m

playerError :: Board2 -> Move2 -> a
playerError b m = error $ "An illegal move has been made: " ++
        (show m) ++ " is not an available move on " ++ (show b) ++
    ". If this was not your player, please report this as a bug."

tryToPlay :: Tree2 -> Move -> Tree2
tryToPlay (Fork2 b tt) m = let m2 = mTm2 (b2Tb b) m
                            in case checkMove tt m2 of
                                Nothing -> playerError b m2
                                Just t -> t

tryToPlay2 :: Tree2 -> Move2 -> Tree2
tryToPlay2 (Fork2 b tt) m = case checkMove tt m of
                Nothing -> playerError b m
                Just t -> t


--          Start    player1    player2    final board
playGame :: Tree2 -> [Move2] -> [Move2] -> Board2
playGame t [] _ = root2 t
playGame (Fork2 b2 ff) (m:ms) opponent = case checkMove ff m of
                    Nothing -> playerError b2 m
                    Just t  -> playGame t opponent ms


testAllowedMoves :: Board2 -> [Move2]
testAllowedMoves b2 = sort
                    . map (mTm2 (b2Tb b2))
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
        t = treeOf $ b2Tb b
        firstWins = pl == bPlayer b

        (goForIt,plName) | firstWins = (testComputerFirst,  "computerFirst")
                         | otherwise = (testComputerSecond, "computerSecond")


testComputerFirst :: Tree -> Bool
testComputerFirst t = checkStrategy computerFirst magicTest2 t

testComputerSecond :: Tree -> Bool
testComputerSecond t = not $ checkStrategy magicTest1 computerSecond t

checkStrategy :: (Tree -> [Move] -> [Move])
              -> (Tree -> [Move] -> [Move])
              -> Tree -> Bool
checkStrategy p1 p2 t = p1wins $ iplay (p1 t) (p2 t)

--First player win is True, second is False
--We can simply check the length: a first player win has an odd number
--of moves, a second an even number
p1wins :: [a] -> Bool
p1wins = odd . length


-- Better testing computerFirst/computerSecond
----------------------------------------------

checkOptimalPlay :: S.Tree -> [S.Move] -> ()
checkOptimalPlay (S.Fork _ []) [] = ()
checkOptimalPlay tree        [] = error $ "The moves end prematurely for the board"
                                       ++ show (bTb2S $ S.root tree)
checkOptimalPlay tree    (m:ms) = case lookup m (S.optimalMoves tree) of
      Nothing       -> error $ "Found a non-optimal move " ++ show (mTm2S m)
                            ++ " in a play for the board " ++ show (bTb2S $ S.root tree)
                            ++ "\nAll optimal moves: " ++ (show $ map fst $ S.optimalMoves tree)
      Just subtree -> checkOptimalPlay subtree ms

testComputersA :: (Player, Board2) -> ()
testComputersA (PH, board2) = checkOptimalPlay (S.treeOf $ bTbS board) moves
  where
    board = b2Tb  board2
    moves = msTmsS' board2 (iplaySSecond board (computerFirst . treeOf) (S.computerSecond . S.treeOf))

testComputersA (PV, board2) = checkOptimalPlay (S.treeOf $ bTbS board) moves
  where
    board  = b2Tb  board2
    moves = msTmsS' board2 (iplaySFirst board (S.computerFirst . S.treeOf) (computerSecond . treeOf))

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

magicTest1 t ms = let t2 = tTt2 t
                      ts = children2 t2
                      b  = root2 t2
                   in if null ts then [] else
                   let mt = ts !! doMagic (length ts)
                       m' = m2Tm (b2Tb b) (fst mt)
                       t' = t2Tt $ snd mt
                    in m' : magicTest2 t' ms

magicTest2 t [] = []
magicTest2 t (m:ms) = let t2 = tTt2 t
                          t' = tryToPlay t2 m
                       in magicTest1 (t2Tt t') ms

                    {-
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
-}

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

testTree :: Board2 -> Tree2
testTree b = normCheckTree2 $ tTt2 $ treeOf (b2Tb b)

------------------------
-- Testing randomness --
------------------------
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
            playGame (tTt2 gameTree) (evens moves) (odds moves) `seq` True

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

rplayS :: Int -> ([a] -> S.LRand [a]) -> ([a] -> S.LRand [a]) -> [a]
rplayS gen f g = iplay (\xs -> S.evalRand (f xs) seed) (\xs -> S.evalRand (g xs) seed)
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

intercal' :: [a] -> [a] -> [a]
intercal' ys []     = ys
intercal' ys (x:xs) = x : intercal xs ys


odds :: [a] -> [a]
odds (x:xs) = evens xs
odds [] = []

evens :: [a] -> [a]
evens (x:xs) = x:odds xs
evens [] = []


whoSMove (M2 pl _) = pl

------------------------
-- Testing heuristics --
------------------------
type Strat  = (Board -> [Move] -> [Move])
type StratS = (S.Board -> [S.Move] -> [S.Move])


-- Preliminaries
----------------
-- (There are a lot of undefineds because we don't need those arguments)

-- moves
mTm2S :: S.Move -> Move2
mTm2S = toMove2 . n3T3Si . S.fromMove' undefined

m2TmS :: Move2 -> S.Move
m2TmS = S.toMove undefined . n3T3S . fromMove2

mTmS :: Board -> Move -> S.Move
mTmS b = S.toMove undefined . n3T3S . fromMove' b

mTmSi :: Board -> S.Move -> Move
mTmSi b = toMove b . n3T3Si . S.fromMove' undefined

msTmsS' :: Board2 -> [Move] -> [S.Move]
msTmsS' b = map m2TmS . msTms2 b


-- boards
bTb2S :: S.Board -> Board2
bTb2S = toBoard2 . n4T4Si . S.fromBoard

b2TbS :: Board2 -> S.Board
b2TbS = S.toBoard . n4T4S . fromBoard2

bTbS :: Board -> S.Board
bTbS = S.toBoard . n4T4S . fromBoard

bTbSi :: S.Board -> Board
bTbSi = toBoard . n4T4Si . S.fromBoard

-- trees
tTt2S :: S.Tree -> Tree2
tTt2S (S.Fork r ch) = Fork2 (bTb2S r) [(mTm2S m, tTt2S t) | (m, t) <- ch]

t2TtS :: Tree2 -> S.Tree
t2TtS (Fork2 r ch) = S.Fork (b2TbS r) [(m2TmS m, t2TtS t) | (m, t) <- ch]

-- players
pTpS :: Player -> S.Player
pTpS PH = S.PH
pTpS PV = S.PV

pTpSi :: S.Player -> Player
pTpSi S.PH = PH
pTpSi S.PV = PV

-- n-tuple conversions
n4T4S  :: (a, b, c, Player)   -> (a, b, c, S.Player)
n4T4S (a,b,c,p) = (a,b,c, pTpS p)

n4T4Si :: (a, b, c, S.Player) -> (a, b, c, Player)
n4T4Si (a,b,c,p) = (a,b,c, pTpSi p)

n3T3S  ::(a, b, Player)   -> (a, b, S.Player)
n3T3S (a,b,p) = (a,b, pTpS p)

n3T3Si ::(a, b, S.Player) -> (a, b, Player)
n3T3Si (a,b,p) = (a,b, pTpSi p)


-- Fixing business
------------------

data LNat = Zero | Succ LNat deriving (Show)

llength [] = Zero
llength (_:xs) = Succ(llength xs)

trace x = error(show x)

droplast [] = []
droplast [x] = []
droplast (x:xs) = x:droplast xs

-- iplay with S playing first:
iplaySFirst :: Board -> (S.Board -> [S.Move] -> [S.Move]) -> (Board -> [Move] -> [Move]) -> [Move]
iplaySFirst board helen ryan = moves
  where -- primed things are in S types
   hs' :: [S.Move]
   hs' = helen board' rs'
   rs :: [Move]
   rs = ryan board hs
   moves :: [Move]
   moves = intercal hs rs
   boards :: [Board]
   boards = f board moves
             where
               f :: Board -> [Move] -> [Board]
               f b xs = b : case xs of
                             [] -> []
                             (m:ms) -> f (play m b) ms
   board' :: S.Board
   board' = bTbS board
   hbs :: [S.Board]
   hbs = map bTbS (evens boards)
   rbs :: [Board]
   rbs = odds boards
   hs :: [Move]
   hs = zipWith (flip f) hs' hbs -- the flipping is for laziness (in the technical sense)
           where
             f :: S.Board -> S.Move -> Move
             f b = mTmSi (bTbSi b)
   rs' :: [S.Move]
   rs' = zipWith (flip f) rs rbs
           where
             f :: Board -> Move -> S.Move
             f = mTmS

-- iplay with S playing second:
iplaySSecond :: Board -> (Board -> [Move] -> [Move]) -> (S.Board -> [S.Move] -> [S.Move]) -> [Move]
iplaySSecond board ryan helen = moves
  where -- primed things are in S types
   hs' :: [S.Move]
   hs' = helen board' rs'
   rs :: [Move]
   rs = ryan board hs
   moves :: [Move]
   moves = intercal rs hs
   boards :: [Board]
   boards = f board moves
             where
               f :: Board -> [Move] -> [Board]
               f b xs = b : case xs of
                             [] -> []
                             (m:ms) -> f (play m b) ms
   board' :: S.Board
   board' = bTbS board
   hbs :: [S.Board]
   hbs = map bTbS (odds boards)
   rbs :: [Board]
   rbs = evens boards
   hs :: [Move]
   hs = zipWith (flip f) hs' hbs -- the flipping is for laziness (in the technical sense)
           where
             f :: S.Board -> S.Move -> Move
             f b = mTmSi (bTbSi b)
   rs' :: [S.Move]
   rs' = zipWith (flip f) rs rbs
           where
             f :: Board -> Move -> S.Move
             f = mTmS

-- Testing logic
----------------

playVrand1 :: Board -> (Strat,StratS) -> [Move]
playVrand1 b (strat1, strats2) = iplaySSecond b strat1 strats2

playVrand2 :: Board -> (Strat,StratS) -> [Move]
playVrand2 b (strat1, strats2) = iplaySFirst b strats2 strat1


--the boolean says whether the player is first or second.
playVrand :: (Bool,Board) -> (Strat, StratS) -> [Move]
playVrand (True,b)  s = playVrand1 b s
playVrand (False,b) s = playVrand2 b s

opp :: Player -> Player
opp PH = PV
opp PV = PH

playerWins :: (Bool,Board2) -> (Strat, StratS) -> (Bool, [Move2])
playerWins (cond,b) strat = ((p1wins game) == cond, msTms2 b game)
    where
        game = playVrand (cond, (b2Tb b)) strat

--Only works for player 1!
--
-- Input: timeout in microseconds.
-- Output: if the play didn't time out: whether the player won, and the time
--         taken in microseconds.
timePlay :: Int -> (Bool, Board2) -> (Strat, StratS) -> IO (Maybe (Bool, Int))
timePlay timeLeft board strategies =
     performGC >> timeout timeLeft play
    where
        --we need to do some exception handling here!
        --TODO do stg?
        play = play'

        play' = do
            startTime <- getCurrentTime

            let result = playerWins board strategies
            putStrLn $ "\nPlaying on the board "++ show (snd board) ++" (as the " ++(if fst board then "first" else "second") ++" player):\n" ++ show (snd result)
            let !winner = fst result

            endTime <- getCurrentTime
            let timeDiff = truncate (endTime `diffUTCTime` startTime)
            return $ (winner, timeDiff)

-- returns list of win/loss with total time taken in microseconds
timePlays :: Int -> [((Bool,Board2), (Strat, Strat), (StratS, StratS))] -> IO ([Bool],Int)
timePlays time0 [] = return ([],time0)
timePlays time0 ((b ,strats1, strats2):ms)  = do
   play1 <- timePlay time0 b strat

   case play1 of
     Nothing -> do
          putStrLn "\nTime is up!"
          return ([],time0) --we treat nothing as timing out.

     Just (player,time) -> do
          putStrLn $ "\tYou " ++ (if player then "WON" else "LOST") ++ " against our AI on board " ++ show (snd b)

          (games, timeRest) <- timePlays time1 ms
          return ((player:games),time1+timeRest)
        where time1 = time0 - time
  where
    strat | fst b == True = (fst strats1, snd strats2)
          | otherwise     = (snd strats1, fst strats2)


testHeuristicsAgainstRandom :: [(Int, (Bool, (Int, Int)))] -> IO Bool
testHeuristicsAgainstRandom inputs = do
        (bools,_) <- timePlays timeLimit allInputs

        let winnedBoards = playsOutcome bools

        -- 3 points every time the heuristics beats the random bot on average
        putStrLn $ "\nCounting points (majority of wins on a board gives 3 points): "
                   ++ show (3 * countWins winnedBoards)

        return True -- ==> we continue with testing
    where
        timeLimit = 6000000
        strats1      = (computerFirstHeuristic, computerSecondHeuristic)
        strats2 seed = (randCover seed (S.randomFirst . S.treeOf), randCover seed (S.randomSecond . S.treeOf))

        randCover :: Int -> (S.Board -> [S.Move] -> S.LRand [S.Move]) -> (S.Board -> [S.Move] -> [S.Move])
        randCover seed f brd = (\xs -> S.evalRand (f brd xs) (mkStdGen seed))

        -- We replicate every input 5-times with 5 slightliy different seeds.
        allInputs = concat $ map repl inputs
        repl i = unfoldr genInp (0,i)

        genInp (n,(gen,(firstWins, dim)))
                | n < 5 = Just (((firstWins, brd), strats1, strats2 gen), (n+1, (gen+1, (firstWins, dim))))
                | otherwise = Nothing
            where
                brd = toBoard2 (fst dim, snd dim, [], PH)

        playsOutcome [] = []
        playsOutcome bools = (countWins (take 5 bools) >= 3) : playsOutcome (drop 5 bools)


-- markHeuristicsVSRandom :: String -> ([(Int, (Bool, (Int, Int)))] -> Bool) -> [[(Int, (Bool, (Int, Int)))]] -> [Bool] -> IO Bool
markHeuristicsVSRandom :: String -> (a -> Bool) -> [a] -> [Bool] -> IO Bool
markHeuristicsVSRandom name f inputs spec = protect (\e -> False) $ do
    -- putStr $ "  [testing] " ++ name ++ "... "
    putStrLn $ "  [testing] against random... "

    testHeuristicsAgainstRandom inps
    where
        -- TODO generate these numbers randomly for actual marking
        inps :: [(Int, (Bool, (Int, Int)))]
        inps = [ (1, (False, (5,4)))
               , (2, (True,  (6,4)))
               , (3, (False, (5,5)))
               , (4, (True,  (6,5)))
               , (5, (True,  (6,6)))
               -- , (5, (True,  (5,6))) -- TODO instead?
               ]

testHeuristicsAgainstMartin :: [(Int, (Bool, (Int, Int)))] -> IO Bool
testHeuristicsAgainstMartin inputs = do
        (bools,_) <- timePlays timeLimit allInputs

        -- 3 points every time the heuristics beats the random bot on average
        putStrLn $ "\nCounting points (win on a board gives 2 points): "
                   ++ show (2 * countWins bools)

        return True -- ==> we continue with testing
    where
        timeLimit = 3000000
        strats1 = (  computerFirstHeuristic,   computerSecondHeuristic)
        strats2 = (S.computerFirstHeuristic, S.computerSecondHeuristic)

        allInputs = map fixInput inputs

        fixInput (gen, (firstWins, dim)) = ((firstWins, brd), strats1, strats2)
            where
                brd' = b2TbS $ B2 dim [] PH
                allowed = S.allowedMoves brd'
                move = allowed !! (gen `mod` length allowed)
                brd = bTb2S $ S.play move brd'


markHeuristicsVSMartin :: String -> (a -> Bool) -> [a] -> [Bool] -> IO Bool
markHeuristicsVSMartin name f inputs spec = protect (\e -> False) $ do
    -- putStr $ "  [testing] " ++ name ++ "... "
    putStrLn $ "  [testing] against Martin... "

    -- TODO hardcode numbers for actual markings
    seeds <- replicateM (length inps) (randomIO :: IO Int)

    testHeuristicsAgainstMartin $ zip seeds inps
    where
        inps :: [((Bool, (Int, Int)))]
        inps = [ (True, (6,5))
               , (True, (6,5))
               , (True, (6,5))
               , (True, (6,5))
               , (True, (6,5))
               ]


notMarking :: Bool
notMarking = error "Marking not implemented for this test, look at the output."

countWins :: [Bool] -> Int
countWins = length . filter (&&True)

-- Shamelessly stolen from QuickCheck.

protect :: (AnException -> a) -> IO a -> IO a
protect f x = do
    e <- tryEvaluateIO x
    case e of
        Left v -> do
            putStr "  An Exception occured: "
            print v
            putStrLn ""

            return $ f v

        Right v -> return v
