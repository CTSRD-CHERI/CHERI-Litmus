Substitute tokens in an assembley code listing.

> module Subst where

Haskell platform imports
========================

> import Data.Char

Tokeniser
=========

Simple tokeniser for assembley code.

> tokenise :: String -> [String]
> tokenise ""      = []
> tokenise (c:cs)
>   | isAlphaNum c = (c : takeWhile isAlphaNum cs)
>                  : tokenise (dropWhile isAlphaNum cs)
>   | otherwise    = [c] : tokenise cs

Join tokens back together to form a valid assembley instruction.

> untokenise :: [String] -> String
> untokenise = concat

Substitution
============

> subst :: [(String, String)] -> [String] -> [String]
> subst s = map (untokenise . map sub . tokenise)
>   where
>     sub token =
>       case lookup token s of
>         Nothing -> token
>         Just x  -> x
