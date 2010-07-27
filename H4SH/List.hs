-- 
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- 
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

--
-- | Useful string functions for command line scripting in h4sh.  Some
-- functions taken from MissingH <http://quux.org/devel/missingh/>, and
-- postings to the Haskell mailing lists. @breakAll@ and @tokens@ are
-- courtesy Simon Marlow. 
--

module H4SH.List (
        breakAll,       -- :: (a -> Bool) -> [a] -> [[a]]
        tokens,         -- :: (a -> Bool) -> [a] -> [[a]]
        matches,        -- :: String -> String -> Bool
        snoc,           -- :: a -> [a] -> [a]
        split,          -- :: Eq a => [a] -> [a] -> [[a]]
        replace,        -- :: Eq a => [a] -> [a] -> [a] -> [a]
        joinBy,         -- :: [a] -> [[a]] -> [a]
        contains,       -- :: [a] -> [a] -> Bool
        breakList,      -- :: ([a] -> Bool) -> [a] -> ([a], [a])
        spanList,       -- :: ([a] -> Bool) -> [a] -> ([a], [a])
        takeWhileList,  -- :: ([a] -> Bool) -> [a] -> [a]
        dropWhileList,  -- :: ([a] -> Bool) -> [a] -> [a]

        (</>), (<.>), (<+>), (<>),

        basename,       -- :: FilePath -> FilePath
        dirname,        -- :: FilePath -> FilePath
  ) where

import Data.List
import Data.Maybe
import Text.Regex

------------------------------------------------------------------------

-- | Splits a list into components delimited by separators, where the
-- predicate returns True for a separator element.  The resulting
-- components do not contain the separators.  Two adjacent separators
-- result in an empty component in the output.  eg.
--
-- @
--   > breakAll (=='a') "aabbaca"
--   ["","","bb","c",""]
-- @
breakAll :: (a -> Bool) -> [a] -> [[a]]
breakAll p s = case rest of
                []     -> [chunk]
                _:rest -> chunk : breakAll p rest
  where (chunk, rest) = break p s

-- | Like 'breakAll', except that sequences of adjacent separators are
-- treated as a single separator. eg.
-- 
-- @
--   > tokens (=='a') "aabbaca"
--   ["bb","c"]
-- @
tokens :: (a -> Bool) -> [a] -> [[a]]
tokens p = filter (not.null) . breakAll p

------------------------------------------------------------------------

-- | Return True if a regular expression matches a String. Useful in
-- list functions that take a predicate. eg.
--
-- @
--   > filter (matches "aabb")
-- @
matches :: String -> String -> Bool
matches p s = isJust (matchRegex (mkRegex p) s)

-- | Reverse cons. Add an element to the back of a list. Example:
--
-- @
-- > snoc 3 [2, 1] ===> [2, 1, 3]
-- @
snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

------------------------------------------------------------------------

-- | Given a delimiter and a list (or string), split into components.
--Example:
--
-- @
--  > split "," "foo,bar,,baz," -> ["foo", "bar", "", "baz", ""]
--  > split "ba" ",foo,bar,,baz," -> [",foo,","r,,","z,"]
-- @
split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in 
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim 
                                                 (drop (length delim) x)

-- | Given a list and a replacement list, replaces each occurance of the search
-- list with the replacement list in the operation list. Example:
--
-- @
--  > replace "," "." "127,0,0,1" -> "127.0.0.1"
-- @
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new l = joinBy new . split old $ l

-- | Given a delimiter and a list of items (or strings), join the items
-- by using the delimiter. Example:
--
-- @
--  > joinBy "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-- @
joinBy :: [a] -> [[a]] -> [a]
joinBy delim l = concat (intersperse delim l)

-- | Returns true if the given parameter is a sublist of the given list;
-- false otherwise. Example:
--
-- @
--  > contains "Haskell" "I really like Haskell." -> True
--  > contains "Haskell" "OCaml is great." -> False
-- @
contains :: Eq a => [a] -> [a] -> Bool
contains [] _ = True                    -- Sub is empty; matches anything
contains _ [] = False                   -- List is empty; matches nothing
contains sub searchlist =
    let testlist = take (length sub) searchlist
        in
        case sub == testlist of
                             True -> True
                             False -> contains sub (tail searchlist)


-- | Similar to Data.List.break, but performs the test on the entire remaining
-- list instead of just one element.
--
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

-- | Similar to Data.List.span, but performs the test on the entire remaining
-- list instead of just one element. 
--
-- @spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@ 
--
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList p xs = (takeWhileList p xs, dropWhileList p xs)

-- | Similar to Data.List.takeWhile, takes elements while the func is true.
-- The function is given the remainder of the list to examine.
takeWhileList :: ([a] -> Bool) -> [a] -> [a]
takeWhileList _ [] = []
takeWhileList func list@(x:xs) =
    if func list 
       then x : takeWhileList func xs
       else []

-- | Similar to Data.List.dropWhile, drops elements while the func is true.
-- The function is given the remainder of the list to examine.
dropWhileList :: ([a] -> Bool) -> [a] -> [a]
dropWhileList _ [] = []
dropWhileList func list@(x:xs) =
    if func list
       then dropWhileList func xs
       else list

------------------------------------------------------------------------

infixr 6 </>
infixr 6 <.>
infixr 6 <+>
infixr 6 <>

--
-- | </>, <.>, <+>, <> : join Strings with various delimiters
--
(</>), (<.>), (<+>), (<>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <+> b = b
a  <+> b = a ++ " " ++ b

[] <> b = b
a  <> b = a ++ b

-- | Drop path component from FilePath
-- @
-- > basename "/f/g/h/i.hs" == "i.hs"
-- > basename "i.hs"        == "i.hs"
-- @
basename :: FilePath -> FilePath
basename p =
    let x = findIndices (== '\\') p
        y = findIndices (== '/') p
    in
    if not $ null x
      then if not $ null y
          then if (maximum x) > (maximum y) then basename' '\\' p else basename' '/' p
          else basename' '\\' p
      else basename' '/' p
    where
        basename' chara pa = reverse $ takeWhile (/= chara) $ reverse pa

-- | Drop file name from path
-- @
-- > dirname ""            == ""
-- > dirname "a.hs"        == "a.hs"
-- > dirname "/f/g/h/a.hs" == "/f/g/h/"
-- @
dirname :: FilePath -> FilePath
dirname p  =
    let x = findIndices (== '\\') p
        y = findIndices (== '/') p
    in
    if not $ null x
      then if not $ null y
          then if (maximum x) > (maximum y) then dirname' '\\' p else dirname' '/' p
          else dirname' '\\' p
      else dirname' '/' p
    where
        dirname' chara pa =
            case reverse $ dropWhile (/= chara) $ reverse pa of
                [] -> "."
                pa' -> pa'
