module Good.Architecture.Inputs.FSRead where

import Good.Prelude

import System.Process (readProcess)

import Good.Architecture.Input

newtype FSRead = FSRead { fsPath :: Text }

getMIME :: MonadIO m => FSRead -> Inputting FSRead m Text
getMIME (FSRead s) = do c <- ask
                        let path = toSL (fsBase c) </> toSL s
                        toSL <$> liftIO (readProcess "file" ["-b", "--mime-type", path] "")

instance Input FSRead where
  data InputConfig FSRead = FSReadConfig { fsBase :: Text }
  data InputState FSRead = FSReadState
  initialInputState = FSReadState
  getRaw (FSRead s) = do c <- ask
                         let path = toSL (fsBase c) </> toSL s
                         liftIO $ readFile path
