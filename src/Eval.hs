{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Runtime
import LispPrelude
import LispVal

import qualified Data.Text as T
import qualified Data.Map               as Map

import qualified Control.Monad.Except as E
import Control.Monad.Reader

import Control.Monad.Identity (Identity)
import Control.Monad.Writer (MonadWriter, Writer, WriterT, tell, writer)

import           Debug.Trace            (trace, traceM)


preludeCtx :: EnvCtx
preludeCtx = EnvCtx
    { env = Map.empty
    , fenv = Map.fromList (prelude <> [("eval", Fun $ IFunc $ unOp eval)])
    }



isLambda :: LispVal -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

ensureAtom :: LispVal -> Eval LispVal
ensureAtom atom@(Atom m) = return atom
ensureAtom x = E.throwError $ SyntaxMismatch $ T.concat ["Expected Atom got ", showVal x]

unAtom :: LispVal -> Eval T.Text
unAtom (Atom t) = return t
unAtom x = E.throwError $ SyntaxMismatch $ T.concat ["Expected Atom got ", showVal x]

eval :: LispVal -> Eval LispVal
eval (List [Atom "quote", val]) = return val
eval num@(Number _) = return num
eval str@(String _) = return str
eval b@(Bool _) = return b
eval l@(List []) = return l
eval nil@Nil = return nil
eval (List [Atom "write", rest]) = return . String . T.pack $ show rest
eval atom@(Atom _) = getVar atom

eval (List [Atom "begin", rest]) = evalBody rest

eval (List (Atom "begin": rest)) = evalBody $ List rest

eval (List [Atom "define", varExpr, defExpr]) = do --top-level define
  EnvCtx{..} <- ask
  varAtom <- ensureAtom varExpr
  evalVal <- eval defExpr
  bindArguments [varExpr] [defExpr] varExpr

eval (List [Atom "if", boolExpr, trueExpr, falseExpr]) = do
    ifResult <- eval boolExpr
    case ifResult of
        Bool True -> eval trueExpr
        Bool False -> eval falseExpr
        e ->
            E.throwError $
            SyntaxMismatch $
            T.concat
                [ "if expects its first argument to evaluate to a boolean expression. Unable to evaluate to boolean: "
                , showVal e
                ]

eval (List [Atom "let", List assignments, body]) = do
    env <- ask
    atoms <- sequence $ ensureAtom <$> odds assignments
    values <- sequence $ eval <$> evens assignments
    bindArguments atoms values body

eval (List (Atom "let":_)) =
    E.throwError $ SyntaxMismatch "let expects a list of parameters and a body: let (x 1 y 2) (+ x y)"

eval (List [Atom "lambda", List params, expr])
    = asks (Lambda (IFunc $ applyLambda expr params))

eval (List [Atom "lambda", _]) = E.throwError $ SyntaxMismatch "lambda expects a list of bound parameters and a body"

eval (List [Atom "debug", rest]) = do
    evaled <- eval rest
    tell [Debug $ showVal evaled]
    return Nil

eval (List (Atom "debug":rest)) = do
    evaled <- eval $ List rest
    tell [Debug $ showVal evaled]
    return Nil

-- function application
eval (List (x:xs)) = do

    funvar <- eval x
    xVal <- mapM eval xs
    outerCtx <- ask
    case funvar of
        (Fun (IFunc fn)) -> fn xVal
        (Lambda (IFunc fn) ctx) -> local (const EnvCtx{fenv = fenv outerCtx, env = env ctx }) (fn xVal)
        _ -> do
            env <- ask
            E.throwError $ SyntaxMismatch $ T.concat ["Not a function :", showVal x]

-- catch everything else as error
eval e = E.throwError $ SyntaxMismatch (T.concat ["Unexpected Token: ", T.pack $ show e])

applyLambda :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
applyLambda body params args = bindArguments params args body

evalBody :: LispVal -> Eval LispVal
evalBody x@(List [List ((:) (Atom "define") [Atom var, defExpr]), rest]) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ insert (Atom var) evalVal ctx) $ eval rest

evalBody x@(List ((:) (List ((:) (Atom "define") [Atom var, defExpr])) rest)) = do
  evalVal <- eval defExpr
  ctx <- ask
  local (const $ insert (Atom var) evalVal ctx) $ evalBody $ List rest

evalBody (List xs) = do
    let evaled = mapM eval xs
    fmap last evaled

evalBody x = eval x

bindArguments :: [LispVal] -> [LispVal] -> LispVal -> Eval LispVal
bindArguments atoms values body = do
    EnvCtx {..} <- ask
    atoms' <- mapM unAtom atoms
    let (env', fenv') = Map.partition (not . isLambda) $ Map.fromList (zip atoms' values)
    let newEnv = EnvCtx {env = env' <> env, fenv = fenv' <> fenv}
    local (const newEnv) (eval body)

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
    EnvCtx {..} <- ask
    case Map.lookup atom env of
        Just x -> return x
        Nothing -> case Map.lookup atom fenv of
            Just x -> return x
            Nothing -> E.throwError $ UnboundVar atom

odds :: [a] -> [a]
odds = odds'
  where
    odds' (x:_:xs) = x : odds' xs
    odds' [x] = [x]
    odds' _ = []

evens :: [a] -> [a]
evens = evens'
  where
    evens' (_:x:xs) = x : evens' xs
    evens' _ = []