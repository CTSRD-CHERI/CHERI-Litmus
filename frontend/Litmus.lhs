Haskell platform imports
========================

> import System.Environment

Local imports
=============

> import Syntax
> import Parse
> import CodeGen

Main
====

> main :: IO ()
> main =
>   do args <- getArgs
>      case args of
>        [input, output_c, output_h] ->
>          do test <- parseLitmusFile input
>             writeFile output_c $ unlines $ genTestBody test
>             writeFile output_h $ unlines $ genTestHeader test
>        other      ->
>          do putStrLn "Usage: litmus <in.litmus> <out.c> <out.h>"

