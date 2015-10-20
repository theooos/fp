{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
module Exercise_typecheck where

import Exercise (
  -- types
  Entry, EntryName, FileProp, Path,

  -- values
  exampleFP,
  exampleEntry1,
  exampleEntry2,
  exampleEntry3,
  cd1,
  cd,
  explode,
  implode,
  lsL,
  lsTree,
  ls,
  listAll,
  cp,
  rm,
  sortTree,
  upcaseStr,
  modifyEntries,
  fibCreate,
  fibEntry,
  findSmallerThan,
  find,
  findSmallerThanPred,
  findSmallerThan2
  )

main :: IO ()
main = do
    return ()

exampleFP_typecheck :: FileProp
exampleFP_typecheck = exampleFP

exampleEntry1_typecheck :: Entry
exampleEntry1_typecheck = exampleEntry1

exampleEntry2_typecheck :: Entry
exampleEntry2_typecheck = exampleEntry2

exampleEntry3_typecheck :: Entry
exampleEntry3_typecheck = exampleEntry3

cd1_typecheck :: Entry -> String -> Maybe Entry
cd1_typecheck = cd1

cd_typecheck :: Entry -> Path -> Maybe Entry 
cd_typecheck = cd

explode_typecheck :: String -> Path
explode_typecheck = explode

implode_typecheck :: Path -> String
implode_typecheck = implode

lsL_typecheck :: Entry -> String
lsL_typecheck = lsL

lsTree_typecheck :: Entry -> String
lsTree_typecheck = lsTree

ls_typecheck :: Int -> Entry -> String
ls_typecheck = ls

listAll_typecheck :: Bool -> Entry -> [String]
listAll_typecheck = listAll

cp_typecheck :: Entry -> (Path, Entry) -> Entry
cp_typecheck = cp

rm_typecheck :: Entry -> Path -> Entry
rm_typecheck = rm

sortTree_typecheck :: Entry -> Entry
sortTree_typecheck = sortTree

upcaseStr_typecheck :: String -> String
upcaseStr_typecheck = upcaseStr

modifyEntries_typecheck :: Entry -> ((EntryName, FileProp) -> (EntryName, FileProp)) -> Entry
modifyEntries_typecheck = modifyEntries

fibCreate_typecheck :: Int -> Entry
fibCreate_typecheck = fibCreate

fibEntry_typecheck :: Entry
fibEntry_typecheck = fibEntry

findSmallerThan_typecheck :: Entry -> Int -> Entry
findSmallerThan_typecheck = findSmallerThan

find_typecheck :: Entry -> ((EntryName, FileProp) -> Bool) -> Entry
find_typecheck = find

findSmallerThanPred_typecheck :: Int -> ((EntryName, FileProp) -> Bool)
findSmallerThanPred_typecheck = findSmallerThanPred

findSmallerThan2_typecheck :: Entry -> Int -> Entry
findSmallerThan2_typecheck = findSmallerThan2
