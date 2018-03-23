module Good.Utilities.Process where

import Good.Prelude

import System.Process.ByteString

import Good.Architecture.Input
import Good.Architecture.Output

import Good.Architecture.Pipeline
import Good.Architecture.Outputs.Console
import Good.Architecture.Outputs.FSWrite

data Process = Process { processPath :: Text, processArgs :: [Text] }

instance Input Process where
    data InputConfig Process = ProcessInputConfig
    data InputState Process = ProcessInputState
    initialInputState = ProcessInputState
    getRaw (Process path args) = liftIO . fmap (\(_, stdout, _) -> stdout) $ readProcessWithExitCode (toSL path) (toSL <$> args) mempty

instance Output Process where
    data OutputConfig Process = ProcessOutputConfig
    data OutputState Process = ProcessOutputState
    initialOutputState = ProcessOutputState
    putRaw (Process path args) d = liftIO . void $ readProcessWithExitCode (toSL path) (toSL <$> args) d
