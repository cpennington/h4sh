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

-- The functions we know how to generate binaries for

module Command where

data Command = 
        Command Name    -- haskell function to use
                Name    -- binary name (for, e.g. !!)
                Type
                Mode
                [String]

data Mode = List | Packed

name :: Command -> Name
name (Command _ f _ _ _) = f

type Name = String

data Type = Pair  Type Type
          | To Type Type        -- ->
          | S                   -- String
          | LS                  -- [String]
          | LI                  -- [Int]
          | I                   -- Int
          | B                   -- Bool
          | MaybeT Type
      deriving (Show, Eq)

------------------------------------------------------------------------

commands :: [Command]
commands = 
  [ Command "tail"    "tl"
        (LS `To` LS)
        Packed
        ["tail :: [a] -> [a]",
        "Extract the elements after the head of a list, which must be non-empty."]

  , Command "init"    "init"
        (LS `To` LS)
        Packed
        ["init :: [a] -> [a]"
        ,"Return all the elements of a list except the last one."
        ,"The list must be finite and non-empty."]

  , Command "reverse" "reverse"
        (LS `To` LS)
        Packed
        ["reverse :: [a] -> [a]"
        ,"reverse returns the elements of the list in reverse order."
        ,"The list must be finite."]

  , Command "nub"     "nub"
        (LS `To` LS)
        Packed
        ["nub :: (Eq a) => [a] -> [a]"
        ,"The 'nub' function removes duplicate elements from a list."
        ,"In particular, it keeps only the first occurrence of each element."]

  , Command "sort"    "srt"
        (LS `To` LS)
        Packed
        ["sort :: (Ord a) => [a] -> [a]"
        ,"The 'sort' function implements a stable sorting algorithm."]

  , Command "id"      "i"
        (S `To` S)
        Packed
        ["id :: a -> a"
        ,"Identity function."]

  , Command "cycle"      "cycle"
        (LS `To` LS)
        List
        ["cycle :: [a] -> [a]"
        ,"'cycle' ties a finite list into a circular one, or equivalently,"
        ,"the infinite repetition of the original list.  It is the identity"
        ,"on infinite lists."]

  , Command "transpose" "transpose"
        (LS `To` LS)
        List
        ["transpose :: [[a]] -> [[a]]"
        ,"The 'transpose' function transposes the rows and columns of its argument."
        ,"For example,"
        ,""
        ,"> transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]"]

------------------------------------------------------------------------

  , Command "concat"  "concat"
        (LS `To` S)
        List
        ["concat :: [[a]] -> [a]"
        ,"Concatenate a list of lists."]

  , Command "unwords"  "unwords"
        (LS `To` S)
        List
        ["unwords :: [String] -> String"
        ,"'unwords' is an inverse operation to 'words'."
        ,"It joins words with separating spaces."]

  , Command "head"    "hd"
        (LS `To` S)
        Packed
        ["head :: [a] -> a"
        ,"Extract the first element of a list, which must be non-empty."]

  , Command "last"    "last"
        (LS `To` S)
        Packed
        ["last :: [a] -> a"
        ,"Extract the last element of a list, which must be finite and non-empty."]

  , Command "minimum"    "min"
        (LS `To` S)
        Packed
        ["minimum :: (Ord a) => [a] -> a"
        ,"'minimum' returns the minimum value from a list,"
        ,"which must be non-empty, finite, and of an ordered type."]

  , Command "maximum"    "max"
        (LS `To` S)
        Packed
        ["maximum :: (Ord a) => [a] -> a"
        ,"'maximum' returns the maximum value from a list,"
        ,"which must be non-empty, finite, and of an ordered type."]

------------------------------------------------------------------------

  , Command "length"  "length"  -- count '\n' is faster
        (LS `To` I)
        Packed
        ["length :: [a] -> Int"
        ,"length returns the length of a finite list as an Int."]

------------------------------------------------------------------------

  , Command "(:[])" "show"
        (S `To` LS)
        Packed
        ["show :: (Show a) => a -> String"
        ,"Convert a value to a readable 'String'."]

  , Command "repeat"  "rpt"
        (S `To` LS)
        List
        ["repeat :: a -> [a]"
        ,"repeat x is an infinite list, with x the value of every element."]

  , Command "words"  "words"
        (S `To` LS)
        List
        ["words :: String -> [String]"
        ,"'words' breaks a string up into a list of words, which were delimited"
        ,"by white space."]

  , Command "map (concat . intersperse \" \") . group"  "group"
        (LS `To` LS)
        List
        ["group :: Eq a => [a] -> [[a]]"
        ,"The 'group' function takes a list and returns a list of lists such"
        ,"that the concatenation of the result is equal to the argument.  Moreover,"
        ,"each sublist in the result contains only equal elements.  For example,"
        ,""
        ,"> group \"Mississippi\" = [\"M\",\"i\",\"ss\",\"i\",\"ss\",\"i\",\"pp\",\"i\"]"
        ,""
        ,"It is a special case of 'groupBy', which allows the programmer to supply"
        ,"their own equality test."]

------------------------------------------------------------------------

  , Command "take"    "take"
        (I `To` (LS `To` LS))
        List
        ["take :: Int -> [a] -> [a]"
        ,"take n, applied to a list xs, returns the prefix of xs"
        ,"of length n, or xs itself if n > length xs."]

  , Command "drop"    "drop"
        (I `To` (LS `To` LS))
        List
        ["drop :: Int -> [a] -> [a]"
        ,"drop n xs returns the suffix of xs"
        ,"after the first n elements, or [] if n > length xs."]

------------------------------------------------------------------------

  , Command "map"     "map"
        ((S `To` S) `To` (LS `To` LS))
        List    -- list is still faster
        ["map :: (a -> b) -> [a] -> [b]"
        ,"map f xs is the list obtained by applying f to each element of xs, i.e.,"
        ,""
        ,"> map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]"
        ,"> map f [x1, x2, ...] == [f x1, f x2, ...]"]

------------------------------------------------------------------------

  , Command "filter"    "filter"
        ((S `To` B) `To` (LS `To` LS))
        List    -- list version if faster
        ["filter :: (a -> Bool) -> [a] -> [a]"
        ,"filter, applied to a predicate and a list, returns the list of"
        ,"those elements that satisfy the predicate."]

  , Command "takeWhile"    "takew"
        ((S `To` B) `To` (LS `To` LS))
        List
        ["takeWhile :: (a -> Bool) -> [a] -> [a]"
        ,"'takeWhile', applied to a predicate p and a list xs, returns the"
        ,"longest prefix (possibly empty) of xs of elements that satisfy p."]

  , Command "dropWhile"    "dropw"
        ((S `To` B) `To` (LS `To` LS))
        List
        ["dropWhile :: (a -> Bool) -> [a] -> [a]"
        ,"'dropWhile' p xs returns the suffix remaining after 'takeWhile' p xs."]

------------------------------------------------------------------------

  , Command "intersperse" "intersperse"
        (S `To` (LS `To` LS))
        Packed
        ["intersperse :: a -> [a] -> [a]"
        ,"The 'intersperse' function takes an element and a list and"
        ,"`intersperses\' that element between the elements of the list."
        ,"For example,"
        ,""
        ,"> intersperse ',' \"abcde\" == \"a,b,c,d,e\""]

  , Command "delete" "delete"
        (S `To` (LS `To` LS))
        Packed
        ["delete :: (Eq a) => a -> [a] -> [a]"
        ,"'delete' x removes the first occurrence of x from its list argument."
        ,"For example,"
        ,""
        ,"> delete 'a' \"banana\" == \"bnana\""]

  , Command "insert" "insert"
        (S `To` (LS `To` LS))
        Packed
        ["insert :: Ord a => a -> [a] -> [a]"
        ,"The 'insert' function takes an element and a list and inserts the"
        ,"element into the list at the last position where it is still less"
        ,"than or equal to the next element.  In particular, if the list"
        ,"is sorted before the call, the result will also be sorted."]

  , Command "(:)" "cons"
        (S `To` (LS `To` LS))
        Packed
        ["(:) :: a -> [a] -> [a]","List constructor"]

------------------------------------------------------------------------

  , Command "elemIndices" "indices"
        (S `To` (LS `To` LI))
        List
        ["elemIndices :: Eq a => a -> [a] -> [Int]"
        ,"The 'elemIndices' function return the"
        ,"indices of all elements equal to the query element, in ascending order."]

------------------------------------------------------------------------

  , Command "flip (!!)" "index"
        (I `To` (LS `To` S))
        Packed
        ["flip (!!) :: Int -> [a] -> a"
        ,"LS index (subscript) operator, starting from 0."]

------------------------------------------------------------------------

        -- is this ok for zip?
  , Command "(zipWith ((. (' ':)) . (++)))" "zp"
        (LS `To` (LS `To` LS))
        List    -- the zipWith code has a char in it
        ["zip :: [a] -> [b] -> [(a,b)]"
        ,"'zip' takes two lists and returns a list of corresponding pairs."
        ,"If one input list is short, excess elements of the longer list are"
        ,"discarded."]

  , Command "union" "union"
        (LS `To` (LS `To` LS))
        Packed
        ["union :: (Eq a) => [a] -> [a] -> [a]"
        ,"The 'union' function returns the list union of the two lists."
        ,"For example,"
        ,""
        ,"> \"dog\" `union` \"cow\" == \"dogcw\""
        ,""
        ,"Duplicates, and elements of the first list, are removed from the"
        ,"the second list, but if the first list contains duplicates, so will"
        ,"the result."]

  , Command "(\\\\)" "difference"
        (LS `To` (LS `To` LS))
        Packed
        ["(\\\\) :: (Eq a) => [a] -> [a] -> [a]"
        ,"The '\\\\' function is list difference ((non-associative)."
        ,"In the result of xs '\\' ys, the first occurrence of each element of"
        ,"ys in turn (if any) has been removed from xs. Thus"
        ,""
        ,"> (xs ++ ys) \\ xs == ys."]

  , Command "intersect" "intersect"
        (LS `To` (LS `To` LS))
        Packed
        ["intersect :: (Eq a) => [a] -> [a] -> [a]"
        ,"The 'intersect' function takes the list intersection of two lists."
        ,"For example,"
        ,""
        ,"> [1,2,3,4] `intersect` [2,4,6,8] == [2,4]"
        ,""
        ,"If the first list contains duplicates, so will the result."]

  , Command "(++)" "append"
        (LS `To` (LS `To` LS))
        Packed
        ["(++) :: [a] -> [a] -> [a]"
        ,"Append two lists, i.e.,"
        ,""
        ,"[x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]"
        ,"[x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]"
        ,""
        ,"the first list is not finite, the result is the first list."]

------------------------------------------------------------------------

  , Command "(\\f -> foldl f [])" "foldl"
        ((S `To` (S `To` S)) `To`(LS `To` S))
        List
        ["foldl :: (a -> b -> a) -> a -> [b] -> a"
        ,"'foldl', applied to a binary operator, a starting value (typically"
        ,"the left-identity of the operator), and a list, reduces the list"
        ,"using the binary operator, from left to right:"
        ,""
        ,"> foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn"
        ,""
        ,"The list must be finite."]

  , Command "(\\f -> foldr f [])" "foldr"
        ((S `To` (S `To` S)) `To`(LS `To` S))
        List
        ["foldr :: (a -> b -> b) -> b -> [a] -> b"
        ,"'foldr', applied to a binary operator, a starting value (typically"
        ,"the right-identity of the operator), and a list, reduces the list"
        ,"using the binary operator, from right to left:"
        ,""
        ,"foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)"
        ]

------------------------------------------------------------------------

  , Command "iterate" "iterate"
        ((S `To` S) `To` (S `To` LS))
        List
        ["iterate :: (a -> a) -> a -> [a]"
        ,"'iterate' f x returns an infinite list of repeated applications"
        ,"of f to x:"
        ,""
        ,"> iterate f x == [x, f x, f (f x), ...]"]

------------------------------------------------------------------------

  , Command "unfoldr" "unfoldr"
        ((S `To` (MaybeT (S `Pair` S))) `To` (S `To` LS))
        List
        ["unfoldr :: (b -> Maybe (a, b)) -> b -> [a]"
        ,"The 'unfoldr' function is a `dual\' to 'foldr': while 'foldr'"
        ,"reduces a list to a summary value, 'unfoldr' builds a list from"
        ,"a seed value.  The function takes the element and returns 'Nothing'"
        ,"if it is done producing the list or returns Just (a,b), in which"
        ,"case, a is a prepended to the list and b is used as the next"
        ,"element in a recursive call.  For example,"
        ,""
        ,"> iterate f == unfoldr (\\x -> Just (x, f x))"
        ,""
        ,"In some cases, 'unfoldr' can undo a 'foldr' operation:"
        ,""
        ,"> unfoldr f' (foldr f z xs) == xs"
        ,""
        ,"if the following holds:"
        ,""
        ,"> f' (f x y) = Just (x,y)"
        ,"> f' z       = Nothing"]

------------------------------------------------------------------------

  , Command "($)" "ap"
        ((S `To` S) `To` (S `To` S))
        List
        ["($) :: (a -> b) -> a -> b"
        ,"Function application."]

------------------------------------------------------------------------

  , Command "concatMap" "concatmap"
        ((S `To` S) `To` (LS `To` S))
        List
        ["concatMap :: (a -> a) -> [a] -> a"
        ,"Map a function over a list and concatenate the results."]

  ]
