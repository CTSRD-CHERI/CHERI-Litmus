Parse a litmus test.

> module Parse (
>   parseLitmusFile      -- :: SourceName -> IO Test
> ) where

Haskell platform imports
========================

> import Text.Parsec
> import Text.Parsec.String (parseFromFile)
> import Data.Char (isDigit)
> import Data.List (transpose)

Local imports
=============

> import Syntax

Parser
======

Takes a string and produces an abstract syntax tree of type 'a'.

> type Parser a = Parsec String () a

Handy little parsers:

> word :: Parser String
> word = many (noneOf " \t\r\n")

> line :: Parser String
> line = many (noneOf "\r\n")

Natural numbers
===============

> natural :: Parser Integer
> natural =
>   do digits <- many1 (satisfy isDigit)
>      return (read digits)

Variables
=========

> varId :: Parser VarId
> varId =
>   do x  <- letter
>      xs <- many alphaNum
>      return (x:xs)

> var :: Parser Var
> var = do pid <- natural
>          spaces
>          char ':'
>          spaces
>          v <- varId
>          return (Reg pid v)
>   <|> do v <- varId
>          return (Mem v)

Pre-conditions
==============

> preConditions :: Parser [Pre]
> preConditions =
>   do char '{'
>      spaces
>      pres <- pre `endBy` (char ';' >> spaces)
>      char '}'
>      return pres

> pre :: Parser Pre
> pre =
>   do v <- var
>      spaces
>      char '='
>      spaces
>      init <- initialValue
>      spaces
>      return (v, init)

> initialValue :: Parser InitialValue
> initialValue =
>       do i <- natural
>          return (Lit i)
>   <|> do v <- varId
>          return (Ptr v)

Post-conditions
===============

> postConditions :: Parser [Post]
> postConditions =
>   do string "exists"
>      spaces
>      char '('
>      spaces
>      posts <- post `sepBy` (string "/\\" >> spaces)
>      char ')'
>      return posts

> post :: Parser Post
> post =
>   do v <- var
>      spaces
>      char '='
>      spaces
>      lit <- natural
>      spaces
>      return (v, lit)

Key=Value parameters
====================

Parse a single Key=Value terminated by newline.  Musn't begin with a
brace because that denotes the beginning of the pre-condition section.

> param :: Parser (String, String)
> param =
>   do notFollowedBy (char '{')
>      key <- many1 (noneOf "=")
>      char '='
>      value <- many (noneOf "\r\n")
>      spaces
>      return (key, value)

Code
====

> codeSection :: Parser [(ProcessId, Code)]
> codeSection =
>   do instrs <- endBy (do notFollowedBy (string "exists")
>                          many (noneOf "|;") `sepBy` char '|')
>                      (char ';' >> many (noneOf "\n") >> char '\n')
>      return $ processIdAndCode (transpose instrs)

Extract the process identifier and code lines for each process:

> processIdAndCode :: [[String]] -> [(ProcessId, Code)]
> processIdAndCode strs
>   | all (not . null) strs
>   = zip (map (getPid . head) strs) (map tail strs)
>   | otherwise = error "Invalid code section"
>   where
>     getPid = strToPid
>            . takeWhile isDigit
>            . dropWhile (== 'P')
>            . dropWhile (/= 'P')
> 
>     strToPid "" = error "Invalid process ID in code section"
>     strToPid s  = read s

Litmus test
===========

> litmusTest :: Parser Test
> litmusTest =
>   do arch <- word
>      spaces
>      name <- word
>      spaces
>      cycle <- line
>      spaces
>      params <- many param
>      pres <- preConditions
>      spaces
>      code <- codeSection
>      spaces
>      posts <- postConditions
>      spaces
>      eof
>      return $
>        Test {
>          testArch   = arch
>        , testName   = name
>        , testCycle  = cycle
>        , testParams = params
>        , testPre    = pres
>        , testCode   = code
>        , testPost   = posts
>        , testGrain  = 1
>        }

Top-level parser
================

> parseLitmusFile :: SourceName -> IO Test
> parseLitmusFile f =
>   do result <- parseFromFile litmusTest f
>      case result of
>        Left e  -> error ("\n\nParse error\n" ++ show e ++ "\n")
>        Right p -> return p
