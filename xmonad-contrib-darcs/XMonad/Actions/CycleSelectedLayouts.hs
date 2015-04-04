-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleSelectedLayouts
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module allows to cycle through the given subset of layouts.
--
-----------------------------------------------------------------------------

module XMonad.Actions.CycleSelectedLayouts (
    -- * Usage
    -- $usage
    cycleThroughLayouts) where

import XMonad
import Data.List (findIndex)
import Data.Maybe (fromMaybe)
import XMonad.Layout.LayoutCombinators (JumpToLayout(..))
import qualified XMonad.StackSet as S

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad hiding ((|||))
-- > import XMonad.Layout.LayoutCombinators ((|||))
-- > import XMonad.Actions.CycleSelectedLayouts
--
-- >   , ((modm,  xK_t ),   cycleThroughLayouts ["Tall", "Mirror Tall"])
--
-- Make sure you are using NewSelect from XMonad.Layout.LayoutCombinators,
-- rather than the Select defined in xmonad core.

cycleToNext :: (Eq a) => [a] -> a -> Maybe a
cycleToNext lst a = do
    -- not beautiful but simple and readable
    ind <- findIndex (a==) lst
    return $ lst !! if ind == length lst - 1 then 0 else ind+1

-- | If the current layout is in the list, cycle to the next layout. Otherwise,
--   apply the first layout from list.
cycleThroughLayouts :: [String] -> X ()
cycleThroughLayouts lst = do
    winset <- gets windowset
    let ld = description . S.layout . S.workspace . S.current $ winset
    let newld = fromMaybe (head lst) (cycleToNext lst ld)
    sendMessage $ JumpToLayout newld
