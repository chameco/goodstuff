module Good.Architecture.Outputs.FSWrite where

import Good.Prelude

import System.Directory
import System.FilePath.Posix

import Good.Architecture.Output

newtype FSWrite = FSWrite { fsPath :: Text }

instance Output FSWrite where
  data OutputConfig FSWrite = FSWriteConfig { fsBase :: Text}
  data OutputState FSWrite = FSWriteState
  initialOutputState = FSWriteState
  putRaw (FSWrite s) d = do c <- ask
                            let p = FSWrite . toSL $ toSL (fsBase c) </> toSL s
                                in do liftIO . createDirectoryIfMissing True . takeDirectory . toSL $ fsPath p
                                      writeFile (toSL $ fsPath p) d
