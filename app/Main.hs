module Main where

import Lib
import Runtime
import Graphics.Gloss
import Debug.Trace
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    s <- readFile "C:\\Users\\Daniel\\Nextcloud\\Programming\\Haskell\\FUS\\myScheme\\test\\fractal-tree.scm"
    let res = runProgram s
    let pathIo = case res of
            Right (val, cmds) -> runPath cmds
            Left err -> do
                traceM $ show err
                return []
    path <- pathIo
    display (InWindow "Lisp" (500, 500) (1280 - 500, 720 - 500)) white (Line ((0,0): path))