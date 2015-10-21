{-# LANGUAGE CPP #-}
{-# LANGUAGE Unsafe #-}
module TestHelpers where

import Data.List hiding (find)
import Data.Char
import Data.Hashable
import Data.Bits

import safe Exercise
     (exampleFP, exampleEntry1, exampleEntry2, exampleEntry3, cd1, cd, explode, implode, lsL, lsTree, ls, listAll, cp, rm, sortTree, upcaseStr, modifyEntries, fibCreate, fibEntry, findSmallerThan, find, findSmallerThanPred, findSmallerThan2, Path, Entry(..), FileProp(..), EntryName(..))



strNorm :: String -> String
strNorm n = reverse
          . dropWhile del
          . reverse
          . dropWhile del
          $ map toLower n
    where
        del = (`elem` " \t\n")

fpNormalise :: FileProp -> FileProp
fpNormalise (FP size content time) = FP size (strNorm content) time

entryNormalise :: ([Entry] -> [Entry]) -> Entry -> Entry
entryNormalise _ (File name fp) = File (strNorm name) $ fpNormalise fp
entryNormalise ultimateFunction (Dir name subEntries) =
        Dir (strNorm name) subEntries'
    where
        subEntries' = ultimateFunction
                    $ map (entryNormalise ultimateFunction) subEntries


testEntries :: [Entry] -> Bool
testEntries ents =
        map fNorm [ exampleEntry1, exampleEntry2, exampleEntry3 ] == map fNorm ents

testFP :: FileProp -> Bool
testFP fp2 = fpNormalise exampleFP == fpNormalise fp2

getName :: Entry -> String
getName (File n _ ) = n
getName (Dir n _)   = n

newtype Seed = Seed Int
  deriving (Read, Show, Eq)

test_cp_fun :: (Entry, (Path, Entry)) -> Entry
test_cp_fun = fNorm . (uncurry cp)

test_rm_fun :: (Entry, Path) -> Entry
test_rm_fun = fNorm . (uncurry rm)

test_cd_fun :: (Entry, Path) -> Maybe Entry
test_cd_fun input = fNorm `fmap` (uncurry cd) input

modifierForSeed :: Seed -> (EntryName, FileProp) -> (EntryName, FileProp)
modifierForSeed (Seed seed) input@(en, (FP size content time)) =
    (en', FP size' content' time')
    where
        h :: Int -> Int
        h x = hash (input, seed, x) .&. 0xfff
        en' = en ++ show (h 1)
        size' = abs (size `xor` (h 2))
        content' = content ++ show (h 3)
        time' = abs (time `xor` (h 4))

selectorForSeed :: Seed -> (EntryName, FileProp) -> Bool
selectorForSeed (Seed seed) input =
    hash (input, seed) .&. 1 == 1

fNorm :: Entry -> Entry
fNorm = entryNormalise youNeedToBeAbleToReadMyMindToBeAbleToUnderstandThisSimpleFunction
    where
        youNeedToBeAbleToReadMyMindToBeAbleToUnderstandThisSimpleFunction = fd . fa

        fa (a:b:xs)
          | a `fg` b  = fb b [a]  xs
          | otherwise = fc  b (a:) xs
        fa xs = [xs]

        fb a as (b:bs)
          | a `fg` b = fb b (a:as) bs
        fb a as bs   = (a:as): fa bs

        fc a as (b:bs)
          | not (a `fg` b) = fc b (\ys -> as (a:ys)) bs
        fc a as bs         = as [a]: fa bs

        fd [x] = x
        fd xs  = fd (fe xs)

        fe (a:b:xs) = ff a b: fe xs
        fe xs       = xs

        ff as@(a:as') bs@(b:bs')
          | a `fg` b  = b:ff as  bs'
          | otherwise = a:ff as' bs
        ff [] bs         = bs
        ff as []         = as

        fg a b = getName a > getName b

sNorm :: String -> String
sNorm =
        -- Notice that there will be always a trailing \n at the end of the
        -- string (for every nonemtpy string) because unlines adds it.
        unlines . fixLastLines . map removeTrailing . lines
    where
        removeTrailing = reverse . dropWhile (== ' ') . reverse
        fixLastLines   = reverse . dropWhile (== "") . reverse

modifyEntries_withRandomFunction :: (Seed, Entry) -> Entry
modifyEntries_withRandomFunction (seed, entry)
  = modifyEntries entry (modifierForSeed seed)

find_withRandomFunction :: (Seed, Entry) -> Entry
find_withRandomFunction (seed, entry)
  = find entry (selectorForSeed seed)

instance Hashable FileProp where
    hashWithSalt salt (FP a b c) = hashWithSalt salt (a, b, c)

implodeAfterExplode :: String -> Bool
implodeAfterExplode s = s == implode (explode s)
