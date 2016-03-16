Generate C code for a litmus test.

> module CodeGen (
>     genTestBody       -- :: Test -> [String]
>   , genTestHeader     -- :: Test -> [String]
> ) where

Haskell platform imports
========================

> import Data.Char
> import Data.List

Local imports
=============

> import Syntax

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

Assembley code
==============

Is the given token a register?

> isReg :: String -> Bool
> isReg ('r':rest) = all isDigit rest
> isReg other      = False

Obtain all register names in a code listing (without duplicates).

> regNames :: Code -> [String]
> regNames = nub . concatMap (filter isReg . tokenise)

Substitute tokens in a code listing.

> subst :: [(String, String)] -> Code -> Code
> subst s = map (untokenise . map sub . tokenise)
>   where
>     sub token =
>       case lookup token s of
>         Nothing -> token
>         Just x  -> x

Obtain the shared variables in a test.

> sharedVars :: Test -> [VarId]
> sharedVars t = nub [w | (v, Ptr w) <- testPre t]

Helpers
=======

Intersperse and concat.

> consperse :: String -> [String] -> String
> consperse x = concat . intersperse x

Lookup and give an error message if not found.

> lookupError :: Eq a => a -> [(a,b)] -> b
> lookupError a abs = maybe (error msg) id (lookup a abs)
>   where msg = "lookupError: key not found"

Find element index and give an error message if not found.

> elemIndexError :: Eq a => a -> [a] -> Int
> elemIndexError a as = maybe (error msg) id (elemIndex a as)
>   where msg = "elemIndexError: element not found"

Indent lines of code.

> indent :: Int -> [String] -> [String]
> indent n = map (replicate n ' ' ++)

Wrap a string in quotes.

> quote :: String -> String
> quote s = "\"" ++ s ++ "\""

Inline assembley
================

Obtain the outcome vector names from a test.

> outcomeVectorNames :: Test -> [Var]
> outcomeVectorNames test = [v | (v, _) <- testPost test]

Obtain the outcome vector values from a test.

> outcomeVectorValues :: Test -> [Integer]
> outcomeVectorValues test = [v | (_, v) <- testPost test]

Return a chunk of inline assembley for a given process in a test.

> inlineAssembley :: Test -> (ProcessId, Code) -> [String]
> inlineAssembley test (pid, code) =
>
>   [ "case " ++ show pid ++ ": {" ] ++
>
>   indent 2
>     [ if null outVars then "" else "var_t " ++ consperse "," outVars ++ ";"
>     , "arch_barrier_up();"
>     , if   testArch test == "RISCV"
>       then "delay(test.delays[" ++ show pid ++ "]*12);"
>       else "delay(test.delays[" ++ show pid ++ "]);"
>     , if   testArch test == "RISCV"
>       then ""
>       else "test.start_times[" ++ show pid ++ "] = arch_get_counter();"
>     , "asm volatile ("
>     ] ++
>
>   indent 4 (map ("\"" ++) $ map (++ "\\n\"") (subst sub code)) ++
>
>   indent 2 (
>     [ ": /* output operands */"
>     , "  " ++ consperse "," ["\"=&r\"(" ++ v ++ ")" | v <- outVars]
>     , ": /* input operands */"
>     , "  " ++ consperse "," ["\"r\"(" ++ input v ++ ")" | v <- inVals]
>     ] ++
>
>     ( if   null tmpRegs then []
>       else [ ": /* clobbered registers */"
>            , "  " ++ consperse ","
>                        (map quote (take (length tmpRegs) tmpNames))
>            ]
>     ) ++
>
>     [ ");" ] ++
>   
>     [ "test.outcome[" ++ show i ++ "] = out"
>            ++ show (elemIndexError v outRegs) ++ ";"
>     | (Reg p v, i) <- zip (outcomeVectorNames test) [0..], p == pid
>     ] ++
>
>     [ "arch_barrier_down();" ] ++
> 
>     ( if   pid /= 0 then []
>       else [ " test.outcome[" ++ show i ++ "] = *test.vars["
>                   ++ show (lookupError v varMap) ++ "];"
>            | (Mem v, i) <- zip (outcomeVectorNames test) [0..]
>            ]
>     ) ++
>
>     [ "break;" ]
>   ) ++
>
>   ["}"]
>
>  where
> 
>     -- Input registers to the assembley code (for this process)
>     inRegs     = [r | (Reg p r, _) <- testPre test, p == pid]
> 
>     -- The value to be passed in for each register
>     -- (Either a literal integer or the address of a shared variable)
>     inVals     = [x | (Reg p r, x) <- testPre test, p == pid]
> 
>     -- Output registers from the assembley code
>     outRegs    = [r | (Reg p r, _) <- testPost test, p == pid]
>     numOutRegs = length outRegs
> 
>     -- C variables where the values of the output registers will be written
>     outVars    = ["out" ++ show i | i <- [0..numOutRegs-1]]
> 
>     -- The registers used in the code, minus inRegs and outRegs
>     tmpRegs    = (regNames code \\ inRegs) \\ outRegs
> 
>     -- Names used to refer to assembley code inputs and outputs
>     argNames   = ["%" ++ show i | i <- [0..]]
> 
>     -- Names for temporary registers (i.e. which are not inputs or outputs)
>     tmpNames   = if   testArch test == "RISCV"
>                  then ["a" ++ show i | i <- [0..7]]
>                  else ["$" ++ show i | i <- [4..25]]
>
>     -- A substitution to be applied the the litmus assembley listing
>     sub        = zip outRegs argNames 
>               ++ zip inRegs (drop numOutRegs argNames)
>               ++ zip tmpRegs tmpNames
>
>     -- Mapping from shared variables to array index
>     varMap     = zip (sharedVars test) [0..]
>
>     input val  =
>       case val of
>         Ptr v -> "test.vars[" ++ show (lookupError v varMap) ++ "]"
>         Lit i -> show i

Translation
===========

> genTestBody :: Test -> [String]
> genTestBody test =
>   [ "/* Automatically generated by litmus */"
>   , ""
>   , "void test_body(int pid) {"
>   , "  switch(pid) {"
>   ] ++
>
>   indent 4 (concat [ inlineAssembley test (pid, code)
>                    | (pid, code) <- testCode test
>                    ]) ++
>
>   [ "  }"
>   , "}"
>   ]

> genTestHeader :: Test -> [String]
> genTestHeader test =
>    [ "/* Automatically generated by litmus */"
>    , ""
>    , "#ifndef _TESTCASE_H_"
>    , "#define _TESTCASE_H_"
>    , ""
>    , "#define NUM_PROCESSES " ++ show numProcesses
>    , "#define NUM_ITERATIONS 1000"
>    , "#define NUM_VARS " ++ show numVars
>    , "#define NUM_LOCS 65536"
>    , "#define LOC_GRAIN " ++ show (testGrain test)
>    , "#define LEN_OUTCOME " ++ show lenOutcome
>    , "#define OUTCOME_NAMES { " ++
>        consperse "," (map quote outcomeNames) ++ " }"
>    , "#define OUTCOME_SOUGHT { " ++
>        consperse "," outcomeVals ++ " }"
>    , if   testArch test == "RISCV"
>      then ""
>      else "#define SHOW_HEADSTARTS"
>    , ""
>    , "#endif"
>    ]
>  where
>    numProcesses = length (testCode test)
>    numVars      = length (sharedVars test)
>    outcomeNames = map show (outcomeVectorNames test)
>    outcomeVals  = map show (outcomeVectorValues test)
>    lenOutcome   = length outcomeNames
