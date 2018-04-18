module Main where

import           Lib
import           System.Environment

main :: IO ()
main = do putStrLn ">> LangFoo - The LangFoo interpreter"
          args <- getArgs
          let filename = if not (null args)
                         then head args
                         else error "Missing source file name"
          src <- readFile (head args)
          putStrLn (">> Interpreting " ++ filename)
          let res = interpret src
          putStrLn (">> Result:")
          putStrLn (show res)
