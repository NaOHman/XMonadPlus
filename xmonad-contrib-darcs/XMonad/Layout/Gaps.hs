{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable, TypeSynonymInstances, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Gaps
-- Copyright   :  (c) 2008 Brent Yorgey
-- License     :  BSD3
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Create manually-sized gaps along edges of the screen which will not
-- be used for tiling, along with support for toggling gaps on and
-- off.
--
-- Note that "XMonad.Hooks.ManageDocks" is the preferred solution for
-- leaving space for your dock-type applications (status bars,
-- toolbars, docks, etc.), since it automatically sets up appropriate
-- gaps, allows them to be toggled, etc.  However, this module may
-- still be useful in some situations where the automated approach of
-- ManageDocks does not work; for example, to work with a dock-type
-- application that does not properly set the STRUTS property, or to
-- leave part of the screen blank which is truncated by a projector,
-- and so on.
-----------------------------------------------------------------------------

module XMonad.Layout.Gaps (
                               -- * Usage
                               -- $usage
                          Direction2D(..), Gaps,
                          GapSpec, gaps, gaps', GapMessage(..)

                          ) where

import XMonad.Core
import Graphics.X11 (Rectangle(..))

import XMonad.Layout.LayoutModifier
import XMonad.Util.Types (Direction2D(..))
import XMonad.Util.XUtils (fi)

import Data.List (delete)

-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Layout.Gaps
--
-- and applying the 'gaps' modifier to your layouts as follows (for
-- example):
--
-- > layoutHook = gaps [(U,18), (R,23)] $ Tall 1 (3/100) (1/2) ||| Full  -- leave gaps at the top and right
--
-- You can additionally add some keybindings to toggle or modify the gaps,
-- for example:
--
-- > , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)  -- toggle all gaps
-- > , ((modm .|. controlMask, xK_t), sendMessage $ ToggleGap U) -- toggle the top gap
-- > , ((modm .|. controlMask, xK_w), sendMessage $ IncGap 5 R)  -- increment the right-hand gap
-- > , ((modm .|. controlMask, xK_q), sendMessage $ DecGap 5 R)  -- decrement the right-hand gap
--
-- If you want complete control over all gaps, you could include
-- something like this in your keybindings, assuming in this case you
-- are using 'XMonad.Util.EZConfig.mkKeymap' or
-- 'XMonad.Util.EZConfig.additionalKeysP' from "XMonad.Util.EZConfig"
-- for string keybinding specifications:
--
-- > ++
-- > [ ("M-g " ++ f ++ " " ++ k, sendMessage $ m d)
-- >     | (k, d) <- [("a",L), ("s",D), ("w",U), ("d",R)]
-- >     , (f, m) <- [("v", ToggleGap), ("h", IncGap 10), ("f", DecGap 10)]
-- > ]
--
-- Given the above keybinding definition, for example, you could type
-- @M-g, v, a@ to toggle the top gap.
--
-- To configure gaps differently per-screen, use
-- "XMonad.Layout.PerScreen" (coming soon).

-- | A manual gap configuration.  Each side of the screen on which a
--   gap is enabled is paired with a size in pixels.
type GapSpec = [(Direction2D,Int)]

-- | The gap state.  The first component is the configuration (which
--   gaps are allowed, and their current size), the second is the gaps
--   which are currently active.
data Gaps a = Gaps GapSpec [Direction2D]
  deriving (Show, Read)

-- | Messages which can be sent to a gap modifier.
data GapMessage = ToggleGaps              -- ^ Toggle all gaps.
                | ToggleGap  !Direction2D    -- ^ Toggle a single gap.
                | IncGap !Int !Direction2D    -- ^ Increase a gap by a certain number of pixels.
                | DecGap !Int !Direction2D    -- ^ Decrease a gap.
  deriving (Typeable)

instance Message GapMessage

instance LayoutModifier Gaps a where
    modifyLayout g w r = runLayout w (applyGaps g r)

    pureMess (Gaps conf cur) m
      | Just ToggleGaps    <- fromMessage m
        = Just $ Gaps conf (toggleGaps conf cur)
      | Just (ToggleGap d) <- fromMessage m
        = Just $ Gaps conf (toggleGap conf cur d)
      | Just (IncGap i d)  <- fromMessage m
        = Just $ Gaps (incGap conf d i) cur
      | Just (DecGap i d)  <- fromMessage m
        = Just $ Gaps (incGap conf d (-i)) cur
      | otherwise = Nothing

applyGaps :: Gaps a -> Rectangle -> Rectangle
applyGaps gs r = foldr applyGap r (activeGaps gs)
  where
    applyGap (U,z) (Rectangle x y w h) = Rectangle x (y + fi z) w (h - fi z)
    applyGap (D,z) (Rectangle x y w h) = Rectangle x y w (h - fi z)
    applyGap (L,z) (Rectangle x y w h) = Rectangle (x + fi z) y (w - fi z) h
    applyGap (R,z) (Rectangle x y w h) = Rectangle x y (w - fi z) h

activeGaps :: Gaps a -> GapSpec
activeGaps (Gaps conf cur) = filter ((`elem` cur) . fst) conf

toggleGaps :: GapSpec -> [Direction2D] -> [Direction2D]
toggleGaps conf [] = map fst conf
toggleGaps _    _  = []

toggleGap :: GapSpec -> [Direction2D] -> Direction2D -> [Direction2D]
toggleGap conf cur d | d `elem` cur            = delete d cur
                     | d `elem` (map fst conf) = d:cur
                     | otherwise               = cur

incGap :: GapSpec -> Direction2D -> Int -> GapSpec
incGap gs d i = map (\(dir,j) -> if dir == d then (dir,max (j+i) 0) else (dir,j)) gs

-- | Add togglable manual gaps to a layout.
gaps :: GapSpec   -- ^ The gaps to allow, paired with their initial sizes.
     -> l a       -- ^ The layout to modify.
     -> ModifiedLayout Gaps l a
gaps g = ModifiedLayout (Gaps g (map fst g))

-- | Add togglable manual gaps to a layout, explicitly specifying the initial states.
gaps' :: [((Direction2D,Int),Bool)] -- ^ The gaps to allow and their initial states.
      -> l a                        -- ^ The layout to modify.
      -> ModifiedLayout Gaps l a
gaps' g = ModifiedLayout (Gaps (map fst g) [d | ((d,_),v) <- g, v])
