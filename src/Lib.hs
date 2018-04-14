module Lib where

import           Eval
import           Parse

interpret :: String -> Val
interpret = eval . parseExpr

