module Saturnal.State where

import Prelude
import Saturnal.Types

type Viewport = { r :: Number, x :: Number, y :: Number }
data State = State Viewport (Array Move) Board
