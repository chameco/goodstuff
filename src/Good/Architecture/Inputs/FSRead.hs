module Good.Architecture.Inputs.FSRead where

import Good.Prelude

import Good.Architecture.Input

newtype FSRead = FSRead { fsPath :: Text }

instance Input FSRead where
  data InputConfig FSRead = FSReadConfig { fsBase :: Text }
  data InputState FSRead = FSReadState
  initialInputState = FSReadState
  getRaw (FSRead s) = do c <- ask;
                         let path = toSL (fsBase c) </> toSL s
                         readFile path
