{-# LANGUAGE OverloadedStrings #-}

module LispPrelude where

import qualified Control.Monad.Except as E
import qualified Control.Monad.Reader
import qualified Control.Monad.Writer
import Control.Monad(foldM)
import qualified Data.Text            as T
import           LispVal

import           Control.Monad.Writer (tell)
import           Runtime

type Prelude = [(T.Text, LispVal)]

type Binary = LispVal -> LispVal -> Eval LispVal

type Unary = LispVal -> Eval LispVal

makeFn :: ([LispVal] -> Eval LispVal) -> LispVal
makeFn = Fun . IFunc

prelude :: Prelude
prelude =
    [ ("+", makeFn $ binFoldOp (numBinOP (+)) (Number 0))
    , ("-", makeFn $ binFoldOp (numBinOP (-)) (Number 0))
    , ("/", makeFn $ binFoldOp (numBinOP div) (Number 1))
    , ("*", makeFn $ binFoldOp (numBinOP (*)) (Number 1))
    , ("<", makeFn $ binOp $ numCmp (<))
    , (">", makeFn $ binOp $ numCmp (>))
    , (">=", makeFn $ binOp $ numCmp (>=))
    , ("<=", makeFn $ binOp $ numCmp (<=))
    , ("neg", makeFn $ unOp $ numUnOp negate)
    , ("even?", makeFn $ unOp $ unNumBool even)
    , ("odd?", makeFn $ unOp $ unNumBool odd)
    , ("neg?", makeFn $ unOp $ unNumBool (< 0))
    , ("pos?", makeFn $ unOp $ unNumBool (> 0))
    , ("zero?", makeFn $ unOp $ unNumBool (== 0))
    , ("null?", makeFn $ unOp (eq Nil))
    , ("and", makeFn $ binFoldOp (boolBinOp (&&)) (Bool True))
    , ("or", makeFn $ binFoldOp (boolBinOp (||)) (Bool False))
    , ("not", makeFn $ unOp $ boolUnOp not)
    , ("cons", makeFn LispPrelude.cons)
    , ("head", makeFn LispPrelude.head)
    , ("tail", makeFn LispPrelude.tail)
    , ("list", makeFn LispPrelude.list)
    , ("eq?", makeFn $ binOp eq)
    , ("string-append", makeFn $ binFoldOp (strBinOp (<>)) (String ""))
    , ("move-forward", makeFn $ unOp fwd)
    , ("turn-left", makeFn $ unOp tlft)
    , ("turn-right", makeFn $ unOp trgt)
    , ("debug", makeFn debug)
    ]

binFoldOp :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binFoldOp op acc args = case args of
    [a,b] -> op a b
    (x:xs) -> foldM op acc (x:xs)
    _ -> E.throwError $ NumArguments 2 args

eq :: LispVal -> LispVal -> Eval LispVal
eq (Number x) (Number y) = return $ Bool (x == y)
eq (String x) (String y) = return $ Bool (x == y)
eq (Bool x) (Bool y) = return $ Bool (x == y)
eq Nil Nil = return $ Bool True
eq (List (x:xs)) (List (y:ys)) = do
    acc <- eq x y
    rest <- eq (List xs) (List ys)
    andCmd acc rest
eq (List []) (List []) = return $ Bool True
eq (List []) (List _) = return $ Bool False
eq (List _) (List []) = return $ Bool False
eq _ _ = return $ Bool False

andCmd :: LispVal -> LispVal -> Eval LispVal
andCmd (Bool True) (Bool True) = return $ Bool True
andCmd (Bool False) (Bool True) = return $ Bool False
andCmd (Bool True) (Bool False) = return $ Bool False
andCmd (Bool False) (Bool False) = return $ Bool False
andCmd x y = E.throwError $ TypeMismatch "boolean operation" (List [x, y])

fwd :: LispVal -> Eval LispVal
fwd (Number int) = do
    tell [Forward int]
    return $ Number int

tlft :: LispVal -> Eval LispVal
tlft (Number int) = do
    tell [TurnLeft int]
    return $ Number int

trgt :: LispVal -> Eval LispVal
trgt (Number int) = do
    tell [TurnRight int]
    return $ Number int

debug :: [LispVal] -> Eval LispVal
debug val = do
    tell [Debug $ showVal $ List val]
    return Nil

noop :: Eval LispVal
noop = do
    tell [NoOp]
    return Nil

unOp :: Unary -> [LispVal] -> Eval LispVal
unOp op [x] = op x
unOp _ args = E.throwError $ NumArguments 2 args

binOp :: Binary -> [LispVal] -> Eval LispVal
binOp op [x, y] = op x y
binOp _ args    = E.throwError $ NumArguments 2 args

numUnOp :: (Integer -> Integer) -> LispVal -> Eval LispVal
numUnOp op (Number x) = return $ Number $ op x
numUnOp op x          = E.throwError $ TypeMismatch "numeric operation" x

numBinOP :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numBinOP op (Number x) (Number y) = return $ Number $ op x y
numBinOP op Nil (Number y) = return $ Number y
numBinOP op (Number x) Nil = return $ Number x
numBinOP op (Number x) y = E.throwError $ TypeMismatch "numeric operation" y
numBinOP op x (Number y) = E.throwError $ TypeMismatch "numeric operation" x
numBinOP op x y = E.throwError $ TypeMismatch "numeric operation" (List [x, y])

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = return $ Bool (op x y)
numCmp op (Number x) y = E.throwError $ TypeMismatch "numeric comparison" y
numCmp op x (Number y) = E.throwError $ TypeMismatch "numeric comparison" x
numCmp op x y = E.throwError $ TypeMismatch "numeric comparison" (List [x, y])

unNumBool :: (Integer -> Bool) -> LispVal -> Eval LispVal
unNumBool op (Number x) = return $ Bool (op x)
unNumBool op x = E.throwError $ TypeMismatch "numeric operation" x

fromBool :: Bool -> LispVal
fromBool True  = Atom "#t"
fromBool False = Atom "#f"

boolBinOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
boolBinOp op (Bool x) (Bool y) = return $ fromBool (op x y)
boolBinOp op (Bool x) y = E.throwError $ TypeMismatch "boolean operation" y
boolBinOp op x (Bool y) = E.throwError $ TypeMismatch "boolean operation" x
boolBinOp op x y = E.throwError $ TypeMismatch "boolean operation" (List [x, y])

boolUnOp :: (Bool -> Bool) -> LispVal -> Eval LispVal
boolUnOp op (Bool x) = return $ Bool (op x)
boolUnOp op x = E.throwError $ TypeMismatch "boolean operation" x

strBinOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strBinOp op (String x) (String y) = return $ String (op x y)
strBinOp op x y = E.throwError $ TypeMismatch "boolean operation" (List [x,y])

cons :: [LispVal] -> Eval LispVal
cons [head, List tail] = return $ List (head:tail)
cons [head, tail] = return $ List [head, tail]
cons _ = E.throwError $ SyntaxMismatch "cons expects list as argument"

head :: [LispVal] -> Eval LispVal
head [] = return Nil
head [List []] = return Nil
head [List (x:_)] = return x
head [x] = return x
head _ = E.throwError $ SyntaxMismatch "head expects list as argument"

tail :: [LispVal] -> Eval LispVal
tail [] = return Nil
tail [List []] = return Nil
tail [List (x:xs)] = return $ List xs
tail _ = E.throwError $ SyntaxMismatch "tail expects list as argument"

list :: [LispVal] -> Eval LispVal
list [] = return Nil
list (x:xs) = do
    tail <- list xs
    cons [x, tail]

