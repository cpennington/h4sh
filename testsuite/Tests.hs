--
-- Test each command against what we expect it should do
--

module Tests where

import List

import Process
import TestFramework
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory

import System.IO.Unsafe
import qualified Control.Exception

------------------------------------------------------------------------

str = unsafePerformIO $ readFile "data"
{-# NOINLINE str #-}

doString2String f n = doCode f n []
doList2List   f n = doCode (unlines . f . lines)            n   []
doList2String f n = doCode ((++ ['\n']) . f . lines)        n   []
doList2Int    f n = doCode ((++ ['\n']) . show . f . lines) n   []
doInt2List2List f n i = doCode (unlines . f i . lines)      n   [show i]
doFun2List2List f n a = doCode (unlines . f . lines)      n   [a]
doString2List2List f n a = doCode (unlines . f . lines)   n [a]
doList2List2List f n  = doCode (unlines . f (lines str) . lines) n ["data"]
doFun2String2String f n a = doCode f n [a]

-- doInfFun2String2List f n a1 a2 = doInfiniteCode (unlines $ f) n ["-e",a1,a2]

doCode f n args = do
        file <-    do let a = "../dist/build/" ++ n
                      b <- doesFileExist a
                      if b then return a
                           else do
                       let a' = "../dist/build/" ++ n ++ "/" ++ n
                       b' <- doesFileExist a'
                       if b' then return a' else error "can't find executables!"

        let expected  = f str
            expected2 = f $ take 10000 str
        (actual,err,_)   <- popen file (args++["data"]) Nothing
        (actual2,err2,_) <- popen file (args++[]) (Just (take 10000 str))
        assertEqual [] err
        assertEqual [] err2
        assertEqual expected actual
        assertEqual expected2 actual2

$(tests "binary" [d| 

 testTail    = doList2List      tail    "tl"
 testInit    = doList2List      init    "init"
 testReverse = doList2List      reverse "reverse"
 testNub     = doList2List      nub     "nub"
 testSort    = doList2List      sort    "srt"
 testId      = doString2String  id      "i"
 testConcat  = doList2String    concat  "concat"
 testHead    = doList2String    head    "hd"
 testLast    = doList2String    last    "last"
 testUnWords = doList2String    unwords "unwords"
 testMinimum = doList2String    minimum "min"
 testMaximum = doList2String    maximum "max"
 testLength  = doList2Int       length  "length"
 testTake    = doInt2List2List  take    "take"    500
 testDrop    = doInt2List2List  drop    "drop"    500
 testMap     = doFun2List2List  (map (show.length)) "map" "show.length"
 testFilter  = doFun2List2List  (filter (\s -> length s == 38)) "filter" "\\s -> length s == 38"

 testIntersperse = doString2List2List (intersperse "XXX") "intersperse" "XXX"

 testDelete  = doString2List2List 
        (delete "so perfect a friend: he is extraordinarily modest, there is no artifice") 
        "delete" "so perfect a friend: he is extraordinarily modest, there is no artifice"

 testInsert = doString2List2List
        (insert "so PERFECT a friend: he is extraordinarily modest, there is no artifice") 
        "insert" "so PERFECT a friend: he is extraordinarily modest, there is no artifice"

 testCons   = doString2List2List
        ((:) "so PERFECT a friend: he is extraordinarily modest, there is no artifice") 
        "cons" "so PERFECT a friend: he is extraordinarily modest, there is no artifice"

 testElemIndices = doString2List2List
        ((\x xs -> map show (elemIndices x xs)) "so PERFECT a friend: he is extraordinarily modest, there is no artifice") 
        "indices" "so PERFECT a friend: he is extraordinarily modest, there is no artifice"

 testDropWhile  = doFun2List2List  
                        (dropWhile (\s -> if null s then False else head s /= 'P')) 
                        "dropw" "\\s -> if null s then False else head s /= 'P'"

 testTakeWhile  = doFun2List2List  
                          (takeWhile (\s -> if null s then False else head s /= 'P')) 
                          "takew" "\\s -> if null s then False else head s /= 'P'"

-- testZip = doList2List2List (zipWith ((. (' ':)) . (++))) "zip"

 testUnion      = doList2List2List union      "union"
 testDifference = doList2List2List (\\)       "difference"
 testIntersect  = doList2List2List intersect  "intersect"
 testAppend     = doList2List2List (++)       "append"

 testFoldl1     = doFun2List2List 
                        ((:[]) . foldl1 (\x y -> y ++ x))
                        "foldl" "\\x y -> y ++ x"


 testFoldr1     = doFun2List2List 
                        ((:[]) . foldr1 (\x y -> y ++ x))
                        "foldr" "\\x y -> y ++ x"

 testAp        = doFun2String2String
                        (\s -> unlines [ show (i,j) | (i,j) <- zip [1..] (lines s) ])
                        "ap" "(\\s -> unlines [ show (i,j) | (i,j) <- zip [1..] (lines s) ])"

 testConcatMap  = doFun2List2List 
                        ((:[]) . concatMap reverse)
                        "concatmap" "reverse"

 |])
    
