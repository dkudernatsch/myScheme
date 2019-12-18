{-# LANGUAGE RecordWildCards #-}

module Runtime where

import qualified Data.Text as T
import Control.Monad.State
import Data.Maybe(catMaybes)

data DrawCmd
    = TurnRight Integer
    | TurnLeft Integer
    | Forward Integer
    | NoOp
    | Debug T.Text
    deriving (Show)

data DrawState = DrawState
    { currentHeading :: Integer
    , currentPos :: (Float, Float)
    }
initState = DrawState 0 (0,0)

runPath:: [DrawCmd] -> IO [(Float, Float)]
runPath cmds = evalStateT (asPath cmds) initState

asPath:: [DrawCmd] -> StateT DrawState IO [(Float, Float)]
asPath cmds = do
    path <- mapM nextCmd cmds
    return $ catMaybes path

nextCmd :: DrawCmd -> StateT DrawState IO (Maybe (Float, Float))
nextCmd cmd = do
    DrawState {..} <- get
    case cmd of
        TurnRight angle -> do
            put (DrawState { currentPos = currentPos, currentHeading = currentHeading + angle})
            return Nothing
        TurnLeft angle -> do
            put (DrawState { currentPos = currentPos, currentHeading = currentHeading - angle})
            return Nothing
        Forward amount -> do
            let newPoint = calcNewPoint currentPos currentHeading amount
            put (DrawState {currentPos = newPoint, currentHeading = currentHeading})
            return $ Just newPoint
        NoOp -> return Nothing
        Debug text -> do
            liftIO $ print text
            return Nothing

calcNewPoint :: (Float, Float) -> Integer -> Integer -> (Float, Float)
calcNewPoint (x, y) dir mov =
    let rads = fromInteger (dir `mod` 360) * (pi / 180)
        dx = sin rads * fromInteger mov
        dy = cos rads * fromInteger mov
     in (x + dx, y + dy)