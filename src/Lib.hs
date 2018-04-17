module Lib where

import           Eval
import           Parse

interpret :: String -> Val
interpret = eval . parseExpr

p0 = " let x = 3 \
     \ in if x = 3 \
     \    then 1 \
     \    else 0"

