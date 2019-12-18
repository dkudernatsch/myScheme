{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    , runProgram
    ) where

import LispParser
import LispVal
import Parser
import Runtime
import Eval

import qualified Data.Text as T
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Writer (runWriter)



someFunc :: IO ()
someFunc = do
    let scheme = "(let (x 10 y 20) (fwd (+ (* x x) y)))"
    let res = runProgram scheme
    print res
    
runProgram :: String -> Either SchemeError (LispVal, [DrawCmd])
runProgram str = do
    let astResult = readExpr $ T.pack str
    do ast <- astResult
       let a = runExceptT $ unEval $ eval ast
       let b = runReaderT a preludeCtx
       let res = runWriter b
       fmap (, snd res) (fst res)