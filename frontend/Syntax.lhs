Abstract syntax of litmus source files.

> module Syntax where

> data Test =
>   Test {
>     testArch   :: String               -- Target architecture
>   , testName   :: String               -- Name
>   , testCycle  :: String               -- Cycle
>   , testParams :: [(String,String)]    -- Key=Value parameters
>   , testPre    :: [Pre]                -- Pre-conditions
>   , testCode   :: [(ProcessId,Code)]   -- Code for each process
>   , testPost   :: [Post]               -- Post-conditions
>   }
>   deriving (Show)

A 'process' is the unit of parallelism, referred to by an id:

> type ProcessId = Integer

A 'variable' is a register or shared memory location, referred to by an id.

> type VarId = String

Pre-conditions
==============

A given location is initialised to a given value:

> type Pre = (Var, InitialValue)

> data Var =
>     Reg ProcessId VarId     -- Register on given process
>   | Mem VarId               -- Shared variable residing in memory
>   deriving (Eq)

An initial value is either a pointer to a variable in memory, or a literal:

> data InitialValue = Ptr VarId | Lit Integer

Post-conditions
===============

A given variable is expected to have a given literal value:

> type Post = (Var, Integer)

Code
====

A block of code is simply a list of assembley instructions:

> type Code = [String]

Show instances
==============

> instance Show Var where
>   show (Reg pid v) = show pid ++ ":" ++ v
>   show (Mem v)     = v

> instance Show InitialValue where
>   show (Ptr v) = v
>   show (Lit i) = show i
