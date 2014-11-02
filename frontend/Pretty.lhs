Pretty-print a litmus file.

> module Pretty (
>   pretty          -- :: Test -> [String]
> ) where

Haskell platform imports
========================

> import Data.List

Local imports
=============

> import Syntax

Helpers
=======

Intersperse and concat.

> consperse :: String -> [String] -> String
> consperse x = concat . intersperse x

Indent lines of code.

> indent :: Int -> [String] -> [String]
> indent n = map (replicate n ' ' ++)

Pad a list of lists so they all have the same length.

> pad :: a -> [[a]] -> [[a]]
> pad x ys = [y ++ replicate (max - length y) x | y <- ys]
>   where
>     max = maximum (map length ys)

Pre-conditions
==============

> prettyPre :: Test -> [String]
> prettyPre test =
>   [ concat [ show (Reg p v) ++ "=" ++ show i ++ "; "
>            | (Reg p v, i) <- pres, p == pid ]
>   | pid <- pids ] ++
>   [ show (Mem v) ++ "=" ++ show i ++ ";" | (Mem v, i) <- pres]
>   where
>     pres = testPre test
>     pids = nub [pid | (Reg pid _, _) <- pres]

Post-conditions
===============

> prettyPost :: Test -> [String]
> prettyPost test =
>   ["(" ++ consperse " /\\ " conds ++ ")"]
>   where
>     posts = testPost test
>     conds = [show v ++ "=" ++ show i | (v, i) <- posts]

Code
====

> prettyCode :: Test -> [String]
> prettyCode test =
>   [ consperse "|" line ++ ";"
>   | line <- transpose codes ]
>   where
>     codes = map (pad ' ') $
>               pad "" [ (" P" ++ show pid) : code
>                      | (pid, code) <- testCode test ]

Pretty print a test
===================

> pretty :: Test -> [String]
> pretty test =
>   [ testArch test ++ " " ++ testName test
>   , testCycle test
>   ] ++
>
>   [lhs ++ "=" ++ rhs | (lhs, rhs) <- testParams test] ++
>
>   [ "{" ] ++ prettyPre test ++ [ "}" ] ++
>
>   prettyCode test ++
>
>   [ "exists" ] ++ prettyPost test
