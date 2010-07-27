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
-- the top level. This is the h4sh 'compiler'
--

import Command

import Data.List
import System.Cmd
import Text.PrettyPrint
import System.Exit
import System.Environment

version :: [Char]
version = "0.3"

data IOMode = MPack | MList
    deriving Eq

--
-- create all src files, and a .cabal file
--
main :: IO ()
main = do
        args <- getArgs
        let flag = case args of
                ["list"]   -> Just MList
                ["clean"]  -> Nothing
                _          -> Just MPack  -- default
        case flag of
            Just f  -> do mapM_ (build f) commands
                          makeCabal f commands
            _       -> mapM_ clean commands

      where
        clean (Command _ f _ _ _) =
              system $ "rm -rf " ++ (concat. intersperse " ")
                                [f,f++".hi",f++".hs",f++".o",f++"*~"]

------------------------------------------------------------------------
--
-- generate a src module with the appropriate wrapper over the function
--
build b c@(Command _ r _ _ _) = writeFile (r++".hs") (generate c b)

--
-- generate a h4sh.cabal file
--
makeCabal :: IOMode -> [Command] -> IO ()
makeCabal iomode cs = writeFile "h4sh.cabal" $ unlines $
  [ "Name:                h4sh"
  , "Version:             " ++ version
  , "License:             GPL"
  , "License-file:        LICENSE"
  , "Author:              Don Stewart"
  , "Maintainer:          dons@cse.unsw.edu.au"
  , "Build-Depends:       base, plugins>=0.9.10, fps>=0.6"
  , "Exposed-modules:	  H4SH.List"
  , ""
  ] ++ map (executable iomode) cs

executable :: IOMode -> Command -> String
executable mode (Command _ n _ _ _) = unlines $
  [ "executable:          " ++ n
  , "main-is:             " ++ n ++ ".hs"
  , "ghc-options:         -O2 -funbox-strict-fields -fglasgow-exts -threaded " ]

------------------------------------------------------------------------
--
-- Top level
--
generate :: Command -> IOMode -> String
generate c@(Command n r ty mode help) iomode = unlines $
  [ "import System.Console.GetOpt"
  , "import System.Environment"
  , "import Control.Monad"
  , "import System.Exit"
  , "import Data.List"
  , unlines $ case mode of
        List -> []
        _ | iomode == MList -> []
          | otherwise       -> ["import System.IO (stdout,stdin)"
                              ,"import qualified Data.ByteString.Char8 as P"]
  , ""
  , render (needsEval ty)
  , ""
  , "version = \"Haskell " ++ r ++ " " ++ version ++ "\""
  , ""
  , "help = " ++ show (unlines help)
  , ""
  , case mode of
        List -> "run f = " ++ render (doRun (dropArgs ty))
        _ | iomode == MList -> "run f = " ++ render (doRun (dropArgs ty))
          | otherwise       -> "runPS f = " ++ render (doRawRun (dropArgs ty))
  , ""
  , "main = do"
  , " args   <- getArgs"
  , " (_,fs) <- parseArgs args"
  , unlines $  case mode of 
        List ->               [ doArgs ty , " putStr $ run " ++ 
                                render (parens $ doApp n ty) ++ " input" ]

        _ | iomode == MList-> [ doArgs ty , " putStr $ run " ++ 
                                render (parens $ doApp n ty) ++ " input" ]

          | otherwise      -> [ doRawArgs ty (psMode iomode)
                                ("P.putStr ((runPS " ++ 
                                (render (parens (doApp n ty)) ++ ") input)")) ]
  , ""
  , parseArgs c
  ]

  where
      psMode MPack = "P.readFile"
      psMode _     = error "psMode: using packed string with list IO!"

------------------------------------------------------------------------
-- work out how to apply arguments to function
--
doApp :: String -> Type -> Doc
doApp n (_ `To` (_ `To` _))  = text n <+> char 'a'
doApp n (_ `To` _)           = text n

-- ---------------------------------------------------------------------
--
-- We've already applied all but the last argument, now, based on the
-- final argument and the return type, work out how to run this function
-- on its input.
--
doRun :: Type -> Doc
doRun (LS `To` LI) = text "unlines . (map show) . f . lines"
doRun (LS `To` I)  = text "(++ ['\\n']) . show . f . lines" 
doRun (LS `To` S)  = text "(++ ['\\n']) . f . lines"
doRun (LS `To` LS) = text "unlines . f . lines"
doRun (S  `To` LS) = text "unlines . f"
doRun (S  `To` S)  = text "f"

------------------------------------------------------------------------

doRawRun :: Type -> Doc
doRawRun (LS `To` S)  = text "(\\x -> P.append x (P.pack \"\\n\")) . f .  P.lines"
doRawRun (LS `To` I)  = text "P.pack . (++ ['\\n']) . show . f . P.lines"
doRawRun (LS `To` LS) = text "P.unlines . f . P.lines"
doRawRun (S  `To` LS) = text "P.unlines . f"
doRawRun (S  `To` S)  = text "f"

------------------------------------------------------------------------

dropArgs :: Type -> Type
dropArgs (_ `To` t@(_ `To` _)) = t
dropArgs t = t

------------------------------------------------------------------------
--
-- Only import System.Eval for binaries that actually need it
--
needsEval :: Type -> Doc
needsEval ((_ `To` _) `To` (_ `To` _)) = text "import System.Eval"
needsEval _ = empty

------------------------------------------------------------------------
--
-- Collect arguments from the command line
-- todo, drop return type, implement pretty instance
--
doArgs :: Type -> String
doArgs ((S `To` (MaybeT _)) `To` _)   = createEvalandArg "String -> Maybe (String,String)"
doArgs ((S `To` S) `To` (S `To` LS))  = createEvalandArg "String -> String"
doArgs ((S `To` (S `To` S)) `To` _)   = createEval "String -> String -> String"
doArgs ((S `To` B)  `To` (LS `To` LS)) = createEval "String -> Bool"
doArgs ((S `To` S)  `To` (LS `To` LS)) = createEval "String -> String"
doArgs ((S `To` S) `To` (LS `To` S))  = createEval "String -> String"
doArgs ((S `To` S) `To` (S `To` S))   = createEval "String -> String"
doArgs (LS `To` (LS `To` LS))         = readFileArgandFileOrStdin
doArgs (S  `To` (LS `To` _))          = readArgAndFileOrStdin
doArgs (I  `To` (LS `To` _))          = readNumberAndFileOrStdin
doArgs (S  `To` LS)                   = readArgumentOrConcatStdin
doArgs (S  `To` S)                    = readFromFileOrStdin
doArgs (LS `To` _)                    = readFromFileOrStdin

doRawArgs :: Type -> String -> String -> String
doRawArgs ((S `To` S) `To` (LS `To` LS)) = createRawEval "String -> String"
doRawArgs ((S `To` B) `To` (LS `To` LS)) = createRawEval "String -> Bool"
doRawArgs (LS `To` (LS `To` LS))         = readFileArgandRawFileOrStdin
doRawArgs (S  `To` (LS `To` _))          = readArgAndRawFileOrStdin
doRawArgs (I  `To` (LS `To` _))          = readNumberAndRawFileOrStdin
doRawArgs (S  `To` S)                    = readFromRawFileOrStdinNoConcat
doRawArgs (S  `To` LS)                   = readRawArgumentOrConcatStdin
doRawArgs (LS `To` _)                    = readFromRawFileOrStdin

------------------------------------------------------------------------
-- The different ways to gather arguments
--

readFromFileOrStdin :: String
readFromFileOrStdin = unlines
  [ " input <- if null fs"
  , "          then getContents"
  , "          else liftM concat $ mapM readFile fs"
  ]

readArgumentOrConcatStdin :: [Char]
readArgumentOrConcatStdin = unlines
    [ " input <- case fs of"
    , "             [] -> liftM (concat.lines) getLine" -- maybe?
    , "             _  -> return $ concat . intersperse \" \" $ fs"
    ]

readArgAndFileOrStdin :: String
readArgAndFileOrStdin = unlines
  [" (a,input)  <- case fs of"
  ,"       []     -> error \"missing argument\""
  ,"       (x:xs) -> do inp <- if null xs"
  ,"                           then getContents"
  ,"                           else liftM concat $ mapM readFile xs"
  ,"                    return (x,inp)"
  ] 

readFileArgandFileOrStdin :: String
readFileArgandFileOrStdin = unlines
  [" (a,input)  <- case fs of"
  ,"       []     -> error \"missing argument\""
  ,"       (x:xs) -> do x'  <- liftM lines (readFile x)"
  ,"                    inp <- if null xs"
  ,"                           then getContents"
  ,"                           else liftM concat $ mapM readFile xs"
  ,"                    return (x',inp)"
  ] 

readNumberAndFileOrStdin :: String
readNumberAndFileOrStdin = unlines
  [ " let (a,fs') = case fs of"
  , "         []     -> error \"missing integer argument\""
  , "         (m:ms) -> case reads m of"
  , "                     [(m',[])] -> (m',ms)"
  , "                     s  -> error $ \"not an Integer argument:\" ++ show s"
  , " input <- if null fs'"
  , "            then getContents"
  , "            else liftM concat $ mapM readFile fs'" ]

--
-- eval a code fragment to a haskell value.
-- there are two ways:
--      1) run the entire src in ghci
--      2) use hs-plugins System.Eval.
-- we take the latter, though there's the extra cost of compilation time
-- (though the performance will be that of compiled code aftewards)
--
createEval :: [Char] -> [Char]
createEval ty = context ++ eflag ++ evalCode ty ++ unlines
  [ " input <- if null fs'"
  , "          then getContents"
  , "          else liftM concat $ mapM readFile fs'" ]

createEvalandArg :: [Char] -> [Char]
createEvalandArg ty = context ++ eflag ++ evalCode ty ++ unlines
  [" let input = concat . intersperse \" \" $ fs'"]

eflag :: String
eflag = unlines
  [ " let (e,fs') = case fs of"
  , "        (e:es) -> (e,es)"
  , "        _      -> error \"missing expression argument\"" ]

-- unsafeEval is ok, as we provide the type. it's also 7% faster
evalCode :: [Char] -> String
evalCode ty = unlines
  [ " a <- do m' <- unsafeEval (\"((\"++e ++\") :: "++ty++")\") context"
  , "         case m' of"
  , "              Nothing -> error \"compilation failed\""
  , "              Just v  -> return (v :: "++ty++")" ]

context :: String
context = unlines
  [ " let context = prehier ++ datas ++ qualifieds ++ controls ++ misc"
  , "     prehier = [\"Char\", \"List\", \"Maybe\", \"Numeric\", \"Random\" ]"
  , "     qualifieds = [\"qualified Data.Map as M\", \"qualified Data.Set as S\"]"
  , "     datas   = map (\"Data.\" ++) ["
  , "                   \"Bits\", \"Bool\", \"Char\", \"Either\","
  , "                   \"Graph\", \"Int\", \"Ix\", \"List\","
  , "                   \"Maybe\", \"Ratio\", \"Tree\", \"Tuple\", \"Typeable\", \"Word\""
  , "                 ]"
  , "     controls = map (\"Control.\" ++) "
  , "        [\"Monad\", \"Monad.Reader\", \"Monad.Fix\", \"Arrow\"]" 
  , "     misc = [\"Text.Regex\", \"H4SH.List\"]"
  ]

-- ---------------------------------------------------------------------

createRawEval :: String -> String -> String -> String
createRawEval ty f a = context ++ eflag ++ evalCode ty ++ unlines
  [ " input <- case fs' of"
  , "   [] -> P.getContents"
  , "   _  -> mapM "++f++" fs' >>= \\ps -> return $ P.concat ps"
  , " " ++ a
  ]

-- ---------------------------------------------------------------------
--
-- ways to get a files using an efficient packed string mechanism
--

readFromRawFileOrStdin f a = unlines
  [ " input <- case fs of"
  , "   [] -> P.getContents"
  , "   _  -> mapM "++f++" fs >>= \\ps -> return $ P.concat ps"
  , " " ++ a
  ]

-- special case to make `i' really fast
readFromRawFileOrStdinNoConcat f a = unlines
  [ " case fs of"
  , "   [] -> P.getContents >>= P.putStr"
  , "   _  -> mapM_ (\\f -> "++f++" f >>= P.putStr) fs"
  ]

readNumberAndRawFileOrStdin f a = unlines
  [ " let (a,fs') = case fs of"
  , "         []     -> error \"missing integer argument\""
  , "         (m:ms) -> case reads m of"
  , "                     [(m',[])] -> (m',ms)"
  , "                     s  -> error $ \"not an Integer argument:\" ++ show s"
  , " input <- case fs' of"
  , "   [] -> P.getContents"
  , "   _  -> mapM "++f++" fs' >>= \\ps -> return $ P.concat ps"
  , " " ++ a
  ]

readArgAndRawFileOrStdin f a = unlines
  [" (a,input)  <- case fs of"
  ,"       []     -> error \"missing argument\""
  ,"       (x:xs) -> do inp <- if null xs"
  ,"                           then P.getContents"
  ,"                           else mapM "++f++" xs >>= \\ps -> return $ P.concat ps"
  ,"                    return (P.pack x,inp)"
  , " " ++ a
  ] 

readFileArgandRawFileOrStdin f a = unlines
  [" (a,input)  <- case fs of"
  ,"       []     -> error \"missing argument\""
  ,"       (x:xs) -> do x'  <- liftM P.lines ("++f++" x)"
  ,"                    inp <- if null xs"
  ,"                           then P.getContents"
  ,"                           else mapM "++f++" xs >>= \\ps -> return $ P.concat ps"
  ,"                    return (x',inp)"
  ," " ++ a
  ] 

readRawArgumentOrConcatStdin :: String -> [Char] -> String
readRawArgumentOrConcatStdin _ a = unlines
    [ " input <- case fs of"
    , "             [] -> liftM (P.pack.concat.lines) getContents" -- ?
    , "             _  -> return $ P.pack . concat . intersperse \" \" $ fs"
    , " " ++ a
    ]

-- ---------------------------------------------------------------------
--
-- generate the getopt code
--
parseArgs :: Command -> String
parseArgs (Command _ f  ty  _ _) = unlines $
  [ "data Flag = Version | Help deriving Eq"
  , ""
  , "opts :: [OptDescr Flag]"
  , "opts = "
  , "  [ Option ['V'] [\"version\"]      (NoArg Version) \"version string\""
  , "  , Option ['h'] [\"help\"]         (NoArg Help)    \"help\""
  , "  ]"
  , ""
  , "parseArgs argv = case (getOpt Permute opts argv) of"
  , " (flags,fs,[]) -> case () of { _"
  , "     | Version `elem` flags -> putStrLn version >> exitWith ExitSuccess"
  , "     | Help    `elem` flags -> putStrLn usage >> putStr help >> exitWith ExitSuccess"
  , "     | otherwise            -> return (flags,fs)"
  , " }"
  , " (_,_,err) -> error $ concat err ++ usage"
  , " where usage = usageInfo (\"Usage: \\\""++f++"\\\" [OPTION...] [file..]\") opts" ]

