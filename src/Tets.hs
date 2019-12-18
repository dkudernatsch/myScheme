module Tets where

import Control.Monad.Trans(lift)
import Control.Monad.Writer (Writer, tell)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Except (ExceptT)

type Test a = ExceptT Error (ReaderT String (Writer [String])) a
data Error = Err | Exception


fn :: Int -> Test Int
fn a = do
    env <- ask
    (lift . tell) [env]
    return 1