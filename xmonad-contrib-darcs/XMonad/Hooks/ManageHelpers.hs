-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageHelpers
-- Copyright    : (c) Lukas Mai
-- License      : BSD
--
-- Maintainer   : Lukas Mai <l.mai@web.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides helper functions to be used in @manageHook@. Here's
-- how you might use this:
--
-- > import XMonad.Hooks.ManageHelpers
-- > main =
-- >     xmonad def{
-- >         ...
-- >         manageHook = composeOne [
-- >             isKDETrayWindow -?> doIgnore,
-- >             transience,
-- >             isFullscreen -?> doFullFloat,
-- >             resource =? "stalonetray" -?> doIgnore
-- >         ],
-- >         ...
-- >     }

module XMonad.Hooks.ManageHelpers (
    Side(..),
    composeOne,
    (-?>), (/=?), (<==?), (</=?), (-->>), (-?>>),
    currentWs,
    isInProperty,
    isKDETrayWindow,
    isFullscreen,
    isDialog,
    pid,
    transientTo,
    maybeToDefinite,
    MaybeManageHook,
    transience,
    transience',
    doRectFloat,
    doFullFloat,
    doCenterFloat,
    doSideFloat,
    doFloatAt,
    doFloatDep,
    doHideIgnore,
    Match,
) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties (getProp32s)

import Data.Maybe
import Data.Monoid

import System.Posix (ProcessID)

-- | Denotes a side of a screen. @S@ stands for South, @NE@ for Northeast
-- etc. @C@ stands for Center.
data Side = SC | NC | CE | CW | SE | SW | NE | NW | C
    deriving (Read, Show, Eq)

-- | A ManageHook that may or may not have been executed; the outcome is embedded in the Maybe
type MaybeManageHook = Query (Maybe (Endo WindowSet))
-- | A grouping type, which can hold the outcome of a predicate Query.
-- This is analogous to group types in regular expressions.
-- TODO: create a better API for aggregating multiple Matches logically
data Match a = Match Bool a

-- | An alternative 'ManageHook' composer. Unlike 'composeAll' it stops as soon as
-- a candidate returns a 'Just' value, effectively running only the first match
-- (whereas 'composeAll' continues and executes all matching rules).
composeOne :: [MaybeManageHook] -> ManageHook
composeOne = foldr try idHook
    where
    try q z = do
        x <- q
        case x of
            Just h -> return h
            Nothing -> z

infixr 0 -?>, -->>, -?>>

-- | q \/=? x. if the result of q equals x, return False
(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

-- | q <==? x. if the result of q equals x, return True grouped with q
(<==?) :: Eq a => Query a -> a -> Query (Match a)
q <==? x = fmap (`eq` x) q
    where
    eq q' x' = Match (q' == x') q'

-- | q <\/=? x. if the result of q notequals x, return True grouped with q
(</=?) :: Eq a => Query a -> a -> Query (Match a)
q </=? x = fmap (`neq` x) q
    where
    neq q' x' = Match (q' /= x') q'

-- | A helper operator for use in 'composeOne'. It takes a condition and an action;
-- if the condition fails, it returns 'Nothing' from the 'Query' so 'composeOne' will
-- go on and try the next rule.
(-?>) :: Query Bool -> ManageHook -> MaybeManageHook
p -?> f = do
    x <- p
    if x then fmap Just f else return Nothing

-- | A helper operator for use in 'composeAll'. It takes a condition and a function taking a grouped datum to action.  If 'p' is true, it executes the resulting action.
(-->>) :: Query (Match a) -> (a -> ManageHook) -> ManageHook
p -->> f = do
    Match b m <- p
    if b then (f m) else mempty

-- | A helper operator for use in 'composeOne'.  It takes a condition and a function taking a groupdatum to action.  If 'p' is true, it executes the resulting action.  If it fails, it returns 'Nothing' from the 'Query' so 'composeOne' will go on and try the next rule.
(-?>>) :: Query (Match a) -> (a -> ManageHook) -> MaybeManageHook
p -?>> f = do
    Match b m <- p
    if b then fmap  Just (f m) else return Nothing

-- | Return the current workspace
currentWs :: Query WorkspaceId
currentWs = liftX (withWindowSet $ return . W.currentTag)

-- | A predicate to check whether a window is a KDE system tray icon.
isKDETrayWindow :: Query Bool
isKDETrayWindow = ask >>= \w -> liftX $ do
    r <- getProp32s "_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR" w
    return $ case r of
        Just [_] -> True
        _ -> False

-- | Helper to check if a window property contains certain value.
isInProperty :: String -> String -> Query Bool
isInProperty p v = ask >>= \w -> liftX $ do
    va <- getAtom v
    r <- getProp32s p w
    return $ case r of
        Just xs -> fromIntegral va `elem` xs
        _ -> False

-- | A predicate to check whether a window wants to fill the whole screen.
-- See also 'doFullFloat'.
isFullscreen :: Query Bool
isFullscreen = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"

-- | A predicate to check whether a window is a dialog.
isDialog :: Query Bool
isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

pid :: Query (Maybe ProcessID)
pid = ask >>= \w -> liftX $ do
    p <- getProp32s "_NET_WM_PID" w
    return $ case p of
        Just [x] -> Just (fromIntegral x)
        _        -> Nothing

-- | A predicate to check whether a window is Transient.
-- It holds the result which might be the window it is transient to
-- or it might be 'Nothing'.
transientTo :: Query (Maybe Window)
transientTo = do
    w <- ask
    d <- (liftX . asks) display
    liftIO $ getTransientForHint d w

-- | A convenience 'MaybeManageHook' that will check to see if a window
-- is transient, and then move it to its parent.
transience :: MaybeManageHook
transience = transientTo </=? Nothing -?>> move
    where
    move mw = maybe idHook (doF . move') mw
    move' w s = maybe s (`W.shift` s) (W.findTag w s)

-- | 'transience' set to a 'ManageHook'
transience' :: ManageHook
transience' = maybeToDefinite transience

-- | converts 'MaybeManageHook's to 'ManageHook's
maybeToDefinite :: MaybeManageHook -> ManageHook
maybeToDefinite = fmap (fromMaybe mempty)


-- | Floats the new window in the given rectangle.
doRectFloat :: W.RationalRect  -- ^ The rectangle to float the window in. 0 to 1; x, y, w, h.
            -> ManageHook
doRectFloat r = ask >>= \w -> doF (W.float w r)

-- | Floats the window and makes it use the whole screen. Equivalent to
-- @'doRectFloat' $ 'W.RationalRect' 0 0 1 1@.
doFullFloat :: ManageHook
doFullFloat = doRectFloat $ W.RationalRect 0 0 1 1

-- | Floats a new window using a rectangle computed as a function of
--   the rectangle that it would have used by default.
doFloatDep :: (W.RationalRect -> W.RationalRect) -> ManageHook
doFloatDep move = ask >>= \w -> doF . W.float w . move . snd =<< liftX (floatLocation w)

-- | Floats a new window with its original size, and its top left
--   corner at a specific point on the screen (both coordinates should
--   be in the range 0 to 1).
doFloatAt :: Rational -> Rational -> ManageHook
doFloatAt x y = doFloatDep move
  where
    move (W.RationalRect _ _ w h) = W.RationalRect x y w h

-- | Floats a new window with its original size on the specified side of a
-- screen
doSideFloat :: Side -> ManageHook
doSideFloat side = doFloatDep move
  where
    move (W.RationalRect _ _ w h) = W.RationalRect cx cy w h
      where cx =      if side `elem` [SC,C ,NC] then (1-w)/2
                 else if side `elem` [SW,CW,NW] then 0
                 else {- side `elem` [SE,CE,NE] -}   1-w
            cy =      if side `elem` [CE,C ,CW] then (1-h)/2
                 else if side `elem` [NE,NC,NW] then 0
                 else {- side `elem` [SE,SC,SW] -}   1-h

-- | Floats a new window with its original size, but centered.
doCenterFloat :: ManageHook
doCenterFloat = doSideFloat C

-- | Hides window and ignores it.
doHideIgnore :: ManageHook
doHideIgnore = ask >>= \w -> liftX (hide w) >> doF (W.delete w)
