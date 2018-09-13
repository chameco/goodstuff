module Good.Architecture.Outputs.Console where

import Good.Prelude

import Good.Architecture.Output

data Console = Console

instance Output Console where
    data OutputConfig Console = ConsoleConfig
    data OutputState Console = ConsoleState
    initialOutputState = ConsoleState
    putRaw _ = putStrLn . toSL
