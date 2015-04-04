{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.BSP
-- Copyright   :  (c) 2013 Ben Weitzman <benweitzman@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Ben Weitzman <benweitzman@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout where new windows will split the focused window in half, based off of BSPWM
--
-----------------------------------------------------------------------------

module XMonad.Layout.BinarySpacePartition (
                                          -- * Usage
                                          -- $usage
                                            emptyBSP
                                          , configBSP
                                          , Rotate(..)
                                          , Swap(..)
                                          , ResizeDirectional(..)
                                          , Direction2D(..)
                                          ) where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack hiding (Zipper)
import XMonad.Util.Types
import qualified Data.Map as M
import Data.List ((\\))
import Control.Monad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.BSP
--
-- Then add the layout, using the default BSP (BSP)
--
-- > myLayout = emptyBSP ||| etc ..
--
-- It will be helpful to add the following key bindings
--
-- > , ((modm .|. altMask,               xK_l     ), sendMessage $ ExpandTowards R)
-- > , ((modm .|. altMask,               xK_h     ), sendMessage $ ExpandTowards L)
-- > , ((modm .|. altMask,               xK_j     ), sendMessage $ ExpandTowards D)
-- > , ((modm .|. altMask,               xK_k     ), sendMessage $ ExpandTowards U)
-- > , ((modm .|. altMask .|. ctrlMask , xK_l     ), sendMessage $ ShrinkFrom R)
-- > , ((modm .|. altMask .|. ctrlMask , xK_h     ), sendMessage $ ShrinkFrom L)
-- > , ((modm .|. altMask .|. ctrlMask , xK_j     ), sendMessage $ ShrinkFrom D)
-- > , ((modm .|. altMask .|. ctrlMask , xK_k     ), sendMessage $ ShrinkFrom U)
-- > , ((modm,                           xK_r     ), sendMessage Rotate)
-- > , ((modm,                           xK_s     ), sendMessage Swap)
--
-- Here's an alternative key mapping, this time using additionalKeysP,
-- arrow keys, and slightly different behavior when resizing windows
--
-- > , ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
-- > , ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
-- > , ("M-M1-<Up>",      sendMessage $ ExpandTowards U)
-- > , ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
-- > , ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
-- > , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
-- > , ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
-- > , ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
-- > , ("M-s",            sendMessage $ BSP.Swap)
-- > , ("M-M1-s",         sendMessage $ Rotate) ]
--

-- |Message for rotating a split in the BSP. Keep in mind that this does not change the order
-- of the windows, it will just turn a horizontal split into a verticial one and vice versa
data Rotate = Rotate deriving Typeable
instance Message Rotate

-- |Message for resizing one of the cells in the BSP
data ResizeDirectional = ExpandTowards Direction2D | ShrinkFrom Direction2D | MoveSplit Direction2D deriving Typeable
instance Message ResizeDirectional

-- |Message for swapping the left child of a split with the right child of split.
-- Keep in mind that it does not change the order of windows and will seem to have bizarre effects
-- if you are not expecting them.
data Swap = Swap deriving Typeable
instance Message Swap

data Axis = Horizontal | Vertical deriving (Show, Read, Eq)

oppositeDirection :: Direction2D -> Direction2D
oppositeDirection U = D
oppositeDirection D = U
oppositeDirection L = R
oppositeDirection R = L

oppositeAxis :: Axis -> Axis
oppositeAxis Vertical = Horizontal
oppositeAxis Horizontal = Vertical

toAxis :: Direction2D -> Axis
toAxis U = Horizontal
toAxis D = Horizontal
toAxis L = Vertical
toAxis R = Vertical

split :: Axis -> Rational -> Rectangle -> (Rectangle, Rectangle)
split Horizontal r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw sh'
    r2 = Rectangle sx (sy + fromIntegral sh') sw (sh - sh')
    sh' = floor $ fromIntegral sh * r
split Vertical r (Rectangle sx sy sw sh) = (r1, r2) where
    r1 = Rectangle sx sy sw' sh
    r2 = Rectangle (sx + fromIntegral sw') sy (sw - sw') sh
    sw' = floor $ fromIntegral sw * r

data Split = Split { axis :: Axis
                   , ratio :: Rational
                   } deriving (Show, Read, Eq)

oppositeSplit :: Split -> Split
oppositeSplit (Split d r) = Split (oppositeAxis d) r

increaseRatio :: Split -> Rational -> Split
increaseRatio (Split d r) delta = Split d (min 0.9 (max 0.1 (r + delta)))

data Tree a = Leaf | Node { value :: a
                          , left :: Tree a
                          , right :: Tree a
                          } deriving (Show, Read, Eq)

numLeaves :: Tree a -> Int
numLeaves Leaf = 1
numLeaves (Node _ l r) = numLeaves l + numLeaves r

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show, Read, Eq)

swapCrumb :: Crumb a -> Crumb a
swapCrumb (LeftCrumb s t) = RightCrumb s t
swapCrumb (RightCrumb s t) = LeftCrumb s t

parentVal :: Crumb a -> a
parentVal (LeftCrumb s _) = s
parentVal (RightCrumb s _) = s

modifyParentVal :: (a -> a) -> Crumb a -> Crumb a
modifyParentVal f (LeftCrumb s t) = LeftCrumb (f s) t
modifyParentVal f (RightCrumb s t) = RightCrumb (f s) t

type Zipper a = (Tree a, [Crumb a])

toZipper :: Tree a -> Zipper a
toZipper t = (t, [])

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Leaf, _) = Nothing
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Leaf, _) = Nothing
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)

goUp :: Zipper a -> Maybe (Zipper a)
goUp (_, []) = Nothing
goUp (t, LeftCrumb x r:cs) = Just (Node x t r, cs)
goUp (t, RightCrumb x l:cs) = Just (Node x l t, cs)

goSibling :: Zipper a -> Maybe (Zipper a)
goSibling (_, []) = Nothing
goSibling z@(_, LeftCrumb _ _:_) = Just z >>= goUp >>= goRight
goSibling z@(_, RightCrumb _ _:_) = Just z >>= goUp >>= goLeft

goToNthLeaf :: Int -> Zipper a -> Maybe (Zipper a)
goToNthLeaf _ z@(Leaf, _) = Just z
goToNthLeaf n z@(t, _) =
  if numLeaves (left t) > n
  then do z' <- goLeft z
          goToNthLeaf n z'
  else do z' <- goRight z
          goToNthLeaf (n - (numLeaves . left $ t)) z'

splitCurrentLeaf :: Rational -> Zipper Split -> Maybe (Zipper Split)
splitCurrentLeaf r (Leaf, []) = Just (Node (Split Vertical r) Leaf Leaf, [])
splitCurrentLeaf r (Leaf, crumb:cs) = Just (Node (Split (oppositeAxis . axis . parentVal $ crumb) r) Leaf Leaf, crumb:cs)
splitCurrentLeaf _ _ = Nothing

removeCurrentLeaf :: Zipper a -> Maybe (Zipper a)
removeCurrentLeaf (Leaf, []) = Nothing
removeCurrentLeaf (Leaf, LeftCrumb _ r:cs) = Just (r, cs)
removeCurrentLeaf (Leaf, RightCrumb _ l:cs) = Just (l, cs)
removeCurrentLeaf _ = Nothing

rotateCurrentLeaf :: Zipper Split -> Maybe (Zipper Split)
rotateCurrentLeaf _ = Nothing

swapCurrentLeaf :: Zipper a -> Maybe (Zipper a)
swapCurrentLeaf (Leaf, []) = Just (Leaf, [])
swapCurrentLeaf (Leaf, c:cs) = Just (Leaf, swapCrumb c:cs)
swapCurrentLeaf _ = Nothing

isAllTheWay :: Direction2D -> Zipper Split -> Bool
isAllTheWay _ (_, []) = True
isAllTheWay R (_, LeftCrumb s _:_)
  | axis s == Vertical = False
isAllTheWay L (_, RightCrumb s _:_)
  | axis s == Vertical = False
isAllTheWay D (_, LeftCrumb s _:_)
  | axis s == Horizontal = False
isAllTheWay U (_, RightCrumb s _:_)
  | axis s == Horizontal = False
isAllTheWay dir z = maybe False id $ goUp z >>= Just . isAllTheWay dir

expandTreeTowards :: Direction2D -> Rational -> Zipper Split -> Maybe (Zipper Split)
expandTreeTowards _ _ z@(_, []) = Just z
expandTreeTowards dir r z 
  | isAllTheWay dir z = shrinkTreeFrom (oppositeDirection dir) r z 
expandTreeTowards R r (t, LeftCrumb s c:cs) 
  | axis s == Vertical = Just (t, LeftCrumb (increaseRatio s r) c:cs)
expandTreeTowards L r (t, RightCrumb s l:cs) 
  | axis s == Vertical = Just (t, RightCrumb (increaseRatio s (-r)) l:cs)
expandTreeTowards D r (t, LeftCrumb s c:cs) 
  | axis s == Horizontal = Just (t, LeftCrumb (increaseRatio s r) c:cs)
expandTreeTowards U r (t, RightCrumb s l:cs) 
  | axis s == Horizontal = Just (t, RightCrumb (increaseRatio s (-r)) l:cs)
expandTreeTowards dir r z = goUp z >>= expandTreeTowards dir r

shrinkTreeFrom :: Direction2D -> Rational -> Zipper Split -> Maybe (Zipper Split)
shrinkTreeFrom _ _ z@(_, []) = Just z
shrinkTreeFrom R r z@(_, LeftCrumb s _:_)
    | axis s == Vertical = Just z >>= goSibling >>= expandTreeTowards L r
shrinkTreeFrom L r z@(_, RightCrumb s _:_)
    | axis s == Vertical = Just z >>= goSibling >>= expandTreeTowards R r
shrinkTreeFrom D r z@(_, LeftCrumb s _:_)
    | axis s == Horizontal = Just z >>= goSibling >>= expandTreeTowards U r
shrinkTreeFrom U r z@(_, RightCrumb s _:_)
    | axis s == Horizontal = Just z >>= goSibling >>= expandTreeTowards D r
shrinkTreeFrom dir r z = goUp z >>= shrinkTreeFrom dir r

-- Direction2D refers to which direction the divider should move.
autoSizeTree :: Direction2D -> Rational -> Zipper Split -> Maybe (Zipper Split)                          
autoSizeTree _ _ z@(_, []) = Just z
autoSizeTree d r z =
    Just z >>= getSplit (toAxis d) >>= resizeTree d r

-- resizing once found the correct split. YOU MUST FIND THE RIGHT SPLIT FIRST.
resizeTree :: Direction2D -> Rational -> Zipper Split -> Maybe (Zipper Split)
resizeTree _ _ z@(_, []) = Just z
resizeTree R r z@(_, LeftCrumb _ _:_) =  
  Just z >>= expandTreeTowards R r
resizeTree L r z@(_, LeftCrumb _ _:_) = 
  Just z >>= shrinkTreeFrom    R r
resizeTree U r z@(_, LeftCrumb _ _:_) = 
  Just z >>= shrinkTreeFrom    D r
resizeTree D r z@(_, LeftCrumb _ _:_) = 
  Just z >>= expandTreeTowards D r
resizeTree R r z@(_, RightCrumb _ _:_) = 
  Just z >>= shrinkTreeFrom    L r
resizeTree L r z@(_, RightCrumb _ _:_) = 
  Just z >>= expandTreeTowards L r
resizeTree U r z@(_, RightCrumb _ _:_) = 
  Just z >>= expandTreeTowards U r
resizeTree D r z@(_, RightCrumb _ _:_) = 
  Just z >>= shrinkTreeFrom    U r

getSplit :: Axis -> Zipper Split -> Maybe (Zipper Split)
getSplit _ (_, []) = Nothing
getSplit d z =
 do let fs = findSplit d z
    if fs == Nothing 
      then findClosest d z
      else fs

findClosest :: Axis -> Zipper Split -> Maybe (Zipper Split)
findClosest _ z@(_, []) = Just z
findClosest d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findClosest d z@(_, RightCrumb s _:_)
  | axis s == d = Just z
findClosest d z = goUp z >>= findClosest d 

findSplit :: Axis -> Zipper Split -> Maybe (Zipper Split)
findSplit _ (_, []) = Nothing
findSplit d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findSplit d z@(_, LeftCrumb s _:_)
  | axis s == d = Just z
findSplit d z = goUp z >>= findSplit d 

top :: Zipper a -> Zipper a
top z = case goUp z of
          Nothing -> z
          Just z' -> top z'

toTree :: Zipper a -> Tree a
toTree = fst . top

index :: W.Stack a -> Int
index s = case toIndex (Just s) of
            (_, Nothing) -> 0
            (_, Just int) -> int

data BSP a = BSP 
    { getDelta :: Rational
    , getRatio :: Rational
    , getTree :: Maybe (Tree Split) 
    } deriving (Show, Read)

-- | an empty BSP to use as a default for adding windows to.
emptyBSP :: BSP a
emptyBSP = BSP 0.05 0.5 Nothing

configBSP :: Rational -> Rational -> BSP a
configBSP d r = BSP d r Nothing

--makeBSP :: Tree Split -> BSP a
--makeBSP t = BSP 0.05 0.5 (Just t)

makeZipper :: BSP a -> Maybe (Zipper Split)
makeZipper BSP {getTree=Nothing} = Nothing
makeZipper BSP {getTree=Just t}  = Just . toZipper $ t

size :: BSP a -> Int
size = maybe 0 numLeaves . getTree

zipperToBSP :: BSP a -> Maybe (Zipper Split) -> BSP b
zipperToBSP b Nothing = b {getTree = Nothing} 
zipperToBSP b (Just z) = b {getTree = Just . toTree . top $ z}

rectangles :: BSP a -> Rectangle -> [Rectangle]
rectangles BSP {getTree=Nothing} _ = []
rectangles BSP {getTree=Just Leaf} rootRect = [rootRect]
rectangles b@BSP {getTree=Just node} rootRect =
    rectangles (makeBSP . left $ node) leftBox ++
    rectangles (makeBSP . right $ node) rightBox
    where (leftBox, rightBox) = split (axis info) (ratio info) rootRect
          makeBSP t = b {getTree=Just t}
          info = value node

doToNth :: (Zipper Split -> Maybe (Zipper Split)) -> BSP a -> Int -> BSP a
doToNth f b n = zipperToBSP b $ makeZipper b >>= goToNthLeaf n >>= f

splitNth :: BSP a -> Int -> BSP a
splitNth b@BSP {getTree=Nothing} _ = b {getTree = Just Leaf} 
splitNth b@BSP {getRatio=r} n = doToNth (splitCurrentLeaf r) b n

removeNth :: BSP a -> Int -> BSP a
removeNth b@BSP {getTree=Nothing} _ = b 
removeNth b@BSP {getTree=Just Leaf} _ = b {getTree = Nothing}
removeNth b n = doToNth removeCurrentLeaf b n

rotateNth :: BSP a -> Int -> BSP a
rotateNth b@BSP {getTree=Nothing} _ = b 
rotateNth b@BSP {getTree=Just Leaf} _ = b
rotateNth b n = doToNth rotateCurrentLeaf b n

swapNth :: BSP a -> Int -> BSP a
swapNth b@BSP {getTree=Nothing} _ = b
swapNth b@BSP {getTree=Just Leaf} _ = b
swapNth b n = doToNth swapCurrentLeaf b n

growNthTowards :: Direction2D -> BSP a -> Int -> BSP a
growNthTowards _ b@BSP {getTree=Nothing} _ = b
growNthTowards _ b@BSP {getTree=Just Leaf} _ = b
growNthTowards dir b@BSP {getDelta=d} n = doToNth (expandTreeTowards dir d) b n

shrinkNthFrom :: Direction2D -> BSP a -> Int -> BSP a
shrinkNthFrom _ b@BSP {getTree=Nothing} _ = b
shrinkNthFrom _ b@BSP {getTree=Just Leaf} _ = b
shrinkNthFrom dir b@BSP {getDelta=d} n = doToNth (shrinkTreeFrom dir d) b n

autoSizeNth :: Direction2D -> BSP a -> Int -> BSP a                    
autoSizeNth _ b@BSP {getTree=Nothing} _ = b
autoSizeNth _ b@BSP {getTree=Just Leaf} _ = b
autoSizeNth dir b@BSP {getDelta=d} n = doToNth (autoSizeTree dir d) b n 

instance LayoutClass BSP a where
  doLayout b r s = return (zip ws rs, layout b) where
    ws = W.integrate s
    layout bsp
      | l == count = Just bsp
      | l > count = layout $ splitNth bsp n
      | otherwise = layout $ removeNth bsp n
      where count = size bsp

    l = length ws
    n = index s
    rs = case layout b of
      Nothing -> rectangles b r
      Just bsp' -> rectangles bsp' r
  handleMessage b m =
    do ms <- (W.stack . W.workspace . W.current) `fmap` gets windowset
       fs <- (M.keys . W.floating) `fmap` gets windowset
       return $ ms >>= unfloat fs >>= handleMesg
    where handleMesg s = msum [fmap (`rotate` s) (fromMessage m)
                              ,fmap (`resize` s) (fromMessage m)
                              ,fmap (`swap` s) (fromMessage m)
                              ]
          unfloat fs s = if W.focus s `elem` fs
                         then Nothing
                         else Just (s { W.up = W.up s \\ fs
                                      , W.down = W.down s \\ fs })
          rotate Rotate s = rotateNth b $ index s
          swap Swap s = swapNth b $ index s
          resize (ExpandTowards dir) s = growNthTowards dir b $ index s
          resize (ShrinkFrom dir) s = shrinkNthFrom dir b $ index s
          resize (MoveSplit dir) s = autoSizeNth dir b $ index s

  description _  = "BSP"
