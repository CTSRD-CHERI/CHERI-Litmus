Translate litmus file from PPC to MIPS.

Haskell platform imports
========================

> import System.Environment

Local imports
=============

> import Parse
> import Syntax
> import Subst
> import Pretty

Instructions
============

Convert a PPC instruction to a MIPS one.

> instrMips :: String -> String
> instrMips s =
>   case s of
>     "lwz"    -> "lw "
>     "stw"    -> "sw "
>     "lwsync" -> errorMessage s
>     "eieio"  -> errorMessage s
>     "isync"  -> errorMessage s
>     other    -> other
>   where
>     errorMessage = error $ "Translation: instruction " ++ s
>                 ++ " not supported on MIPS"

Litmus tests
============

> translate :: Test -> Test
> translate test =
>   test { testArch = "MIPS"
>        , testCode = [ (pid, map (untokenise . map instrMips . tokenise) c)
>                     | (pid, c) <- testCode test ]
>        }

Main
====

> main :: IO ()
> main =
>   do args <- getArgs
>      case args of
>        [input] ->
>          do test <- parseLitmusFile input
>             putStrLn $ unlines $ pretty $ translate test
>        [input, output] ->
>          do test <- parseLitmusFile input
>             writeFile output $ unlines $ pretty $ translate test
>        other      ->
>          do putStrLn $
>               unlines [
>                 "Usage: litmus-translate <input.litmus> <output.litmus>"
>               , "(If no output file is specified, stdout is assumed)"
>               ]
