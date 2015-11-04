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
>        input:output_c:output_h:rest ->
>          do test <- parseLitmusFile input
>             let test' = test { testGrain = getGrain rest }
>             writeFile output_c $ unlines $ genTestBody test'
>             writeFile output_h $ unlines $ genTestHeader test'
>        other      ->
>          do putStrLn "Usage: litmus <in.litmus> <out.c> <out.h>"

Options
=======

> getOpt :: [String] -> [(String, String)]
> getOpt strs = map split strs
>   where
>     split s = (takeWhile (/= '=') s, drop 1 (dropWhile (/= '=') s))

> getGrain :: [String] -> Int
> getGrain strs =
>   case [read v | (k, v) <- getOpt strs, k == "grain"] of
>     [] -> 1
>     n:_  -> n
