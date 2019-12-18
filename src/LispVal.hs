{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module LispVal where

import qualified Data.Map               as Map
import qualified Data.Text              as T

import qualified Control.Monad.Except   as E
import           Control.Monad.Reader

import           Control.Monad.Identity (Identity)
import           Control.Monad.Writer   (MonadWriter, Writer, WriterT, tell,
                                         writer)
import           Runtime

data LispVal
    = Atom T.Text
    | List [LispVal]
    | Number Integer
    | String T.Text
    | Fun IFunc
    | Lambda IFunc EnvCtx
    | Nil
    | Bool Bool
    deriving (Show)

instance Show IFunc where
    show _ = "(internal function)"

showVal :: LispVal -> T.Text
showVal val =
    case val of
        Atom atom  -> atom
        String str -> T.concat ["\"", str, "\""]
        Number num -> T.pack $ show num
        Bool True  -> "#t"
        Bool False -> "#f"
        Nil        -> "Nil"
        List xs    -> T.concat ["(", T.unwords $ showVal <$> xs, ")"]
        Fun _      -> "(internal function)"
        Lambda _ _ -> "(lambda function)"

newtype IFunc =
    IFunc
        { fn :: [LispVal] -> Eval LispVal
        }

type ValueContext = Map.Map T.Text LispVal

type FunctionContext = Map.Map T.Text LispVal

data EnvCtx =
    EnvCtx
        { env  :: ValueContext
        , fenv :: FunctionContext
        } deriving (Show)

insert :: LispVal -> LispVal -> EnvCtx -> EnvCtx
insert (Atom atom) valVar oldCtx = case valVar of
    Fun _ -> oldCtx {fenv = Map.insert atom valVar (fenv oldCtx) }
    Lambda _ _ -> oldCtx {fenv = Map.insert atom valVar (fenv oldCtx) }
    _ -> oldCtx {env = Map.insert atom valVar (env oldCtx)}
insert _ _ ctx = ctx


newtype Eval a =
    Eval
        { unEval :: E.ExceptT SchemeError (ReaderT EnvCtx (Writer [DrawCmd])) a
        }
    deriving (Functor, Applicative, Monad, MonadReader EnvCtx, E.MonadError SchemeError, MonadWriter [DrawCmd])

data SchemeError
    = UnboundVar T.Text
    | SyntaxMismatch T.Text
    | NumArguments Int [LispVal]
    | Err T.Text
    | TypeMismatch T.Text LispVal
    deriving (Show)
