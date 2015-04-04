-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.FlexibleResize
-- Copyright   :  (c) Lukas Mai
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  <l.mai@web.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Resize floating windows from any corner.
--
-----------------------------------------------------------------------------

module XMonad.Actions.FlexibleResize (
        -- * Usage
        -- $usage
        XMonad.Actions.FlexibleResize.mouseResizeWindow,
        XMonad.Actions.FlexibleResize.mouseResizeEdgeWindow
) where

import XMonad
import XMonad.Util.XUtils (fi)
import Foreign.C.Types

-- $usage
-- To use, first import this module into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import qualified XMonad.Actions.FlexibleResize as Flex
--
-- Then add an appropriate mouse binding:
--
-- >     , ((modm, button3), (\w -> focus w >> Flex.mouseResizeWindow w))
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".

-- | Resize a floating window from whichever corner the mouse is
--   closest to.
mouseResizeWindow
  :: Window -- ^ The window to resize.
  -> X ()
mouseResizeWindow = mouseResizeEdgeWindow 0


-- | Resize a floating window from whichever corner or edge the mouse is
--   closest to.
mouseResizeEdgeWindow
  :: Rational -- ^ The size of the area where only one edge is resized.
  -> Window   -- ^ The window to resize.
  -> X ()
mouseResizeEdgeWindow edge w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    sh <- io $ getWMNormalHints d w
    (_, _, _, _, _, ix, iy, _) <- io $ queryPointer d w
    let
        [pos_x, pos_y, width, height] = map (fi . ($ wa)) [wa_x, wa_y, wa_width, wa_height]
        west  = findPos ix width
        north = findPos iy height
        (cx, fx, gx) = mkSel west  width  pos_x
        (cy, fy, gy) = mkSel north height pos_y
    io $ warpPointer d none w 0 0 0 0 cx cy
    mouseDrag (\ex ey -> do let (nw,nh) = applySizeHintsContents sh (gx ex, gy ey)
                            io $ moveResizeWindow d w (fx nw) (fy nh) nw nh)
              (float w)
    where
    findPos :: CInt -> Position -> Maybe Bool
    findPos m s = if p < 0.5 - edge/2
                  then Just True
                  else if p < 0.5 + edge/2
                       then Nothing
                       else Just False
                  where p = fi m / fi s
    mkSel :: Maybe Bool -> Position -> Position -> (Position, Dimension -> Position, Position -> Dimension)
    mkSel b k p = case b of
                      Just True ->  (0, (fi k + fi p -).fi, (fi k + fi p -).fi)
                      Nothing ->    (k `div` 2, const p, const $ fi k)
                      Just False -> (k, const p, subtract (fi p) . fi)
