-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Float.SimplestFloatDec
-- Copyright   :  (c) 2015 Jeffrey Lyman
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  JLyman@macalester.edu
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows users to resize floating windows by dragging their boarders


module XMonad.Hooks.BorderResize 
    ( borderResizesFloat ) where

import XMonad
import Data.Monoid
import Control.Monad
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- $usage
-- 'handleEventHook = borderResizesFloat ...'

data DragDir = R | L | U | D | UR | UL | DR | DL 
    deriving (Show, Eq)

borderResizesFloat :: Event -> X All
borderResizesFloat e@(ButtonEvent {ev_event_type = t, 
                    ev_button = b, ev_subwindow = sw,
                    ev_window = ew, ev_root = er}) = do
    when (t == buttonPress && b == 1 && er == ew && sw /= 0) $ do
        (WindowAttributes wx wy ww wh bw _ _)
            <- withDisplay $ \d -> io $ getWindowAttributes d sw
        let x = fi $ ev_x e
            y = fi $ ev_y e
            r = Rectangle (fi wx) (fi wy) (fi ww) (fi wh)
        whenJust (direction r (fi bw) x y) $ \dir -> do
            d <- asks display
            io $ raiseWindow d sw
            startDraggingDec sw
            dragHandler <- whileDraggingDec sw
            let makeRecs = dragAlong dir x y r (fi bw)
            mouseDrag (\ex ey -> let (or, dr) = makeRecs ex ey
                            in moveWin sw or >> dragHandler dr) 
                      (float sw >> finishDraggingDec sw)
    return (All True)
borderResizesFloat _ = return (All True)

direction :: Rec -> Pos -> Pos -> Pos -> Maybe DragDir
direction (Rectangle x y ww wh) bw ex ey
    | ex < x+bw && ey < y + (2*bw) = Just UL
    | ex < x+bw && ey > h - (2*bw) = Just DL
    | ex > w-bw && ey < y + (2*bw) = Just UR
    | ex > w-bw && ey > h - (2*bw) = Just DR
    | ex < x+bw = Just L
    | ex > w-bw = Just R
    | ey < y+bw = Just U
    | ey > h-bw = Just D
    | otherwise = Nothing
    where w = fi ww + fi x
          h = fi wh + fi y

moveWin :: Window -> Rec -> X ()
moveWin win (Rectangle wx wy ww wh) =
    withDisplay $ \d -> io $ moveResizeWindow d win wx wy ww wh

dragAlong :: DragDir -> Pos -> Pos -> Rec -> Pos -> Pos -> Pos -> (Rec, Rec)
dragAlong d ox oy (Rectangle x y ww wh) bw ex ey
    | d == D  = posRec  x        y        w       (h + dy) bw 
    | d == R  = posRec  x        y       (w + dx)  h       bw
    | d == U  = posRec  x       (y + dy)  w       (h - dy) bw
    | d == L  = posRec (x + dx)  y       (w - dx)  h       bw
    | d == DR = posRec  x        y       (w + dx) (h + dy) bw
    | d == UR = posRec  x       (y + dy) (w + dx) (h - dy) bw
    | d == DL = posRec (x + dx)  y       (w - dx) (h + dy) bw
    | d == UL = posRec (x + dx) (y + dy) (w - dx) (h - dy) bw
    where  dx = ex - ox
           dy = ey - oy
           h  = fi wh
           w  = fi ww

posRec :: Pos -> Pos -> Pos -> Pos -> Pos -> (Rec, Rec)
posRec x y w h bw 
    | x < min = posRec min y w h bw 
    | y < min = posRec x min w h bw
    | w < min = posRec x y min h bw
    | h < min = posRec x y w min bw
    | otherwise = (Rectangle x y (fi w) (fi h), 
                   Rectangle x y (fi (w + 2*bw)) (fi (h + 2*bw)))
    where min = 1 + (2 * bw)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

type Pos = Position
type Rec = Rectangle
