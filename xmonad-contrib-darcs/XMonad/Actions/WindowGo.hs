{- |
Module      :  XMonad.Actions.WindowGo
License     :  Public domain

Maintainer  :  <gwern0@gmail.com>
Stability   :  unstable
Portability :  unportable

Defines a few convenient operations for raising (traveling to) windows based on XMonad's Query
monad, such as 'runOrRaise'. runOrRaise will run a shell command unless it can
find a specified window; you would use this to automatically travel to your
Firefox or Emacs session, or start a new one (for example), instead of trying to
remember where you left it or whether you still have one running. -}

module XMonad.Actions.WindowGo (
                 -- * Usage
                 -- $usage
                 raise,
                 raiseNext,
                 runOrRaise,
                 runOrRaiseNext,
                 raiseMaybe,
                 raiseNextMaybe,

                 raiseBrowser,
                 raiseEditor,
                 runOrRaiseAndDo,
                 runOrRaiseMaster,
                 raiseAndDo,
                 raiseMaster,

                 ifWindows,
                 ifWindow,
                 raiseHook,
                 module XMonad.ManageHook
                ) where

import Control.Monad
import Data.Char (toLower)
import Data.Monoid
import XMonad (Query(), X(), ManageHook, withWindowSet, runQuery, liftIO, ask)
import Graphics.X11 (Window)
import XMonad.ManageHook
import XMonad.Operations (windows)
import XMonad.Prompt.Shell (getBrowser, getEditor)
import qualified XMonad.StackSet as W (allWindows, peek, swapMaster, focusWindow)
import XMonad.Util.Run (safeSpawnProg)
{- $usage

Import the module into your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Actions.WindowGo

and define appropriate key bindings:

> , ((modm .|. shiftMask, xK_g), raise (className =? "Firefox"))
> , ((modm .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))

(Note that Firefox v3 and up have a class-name of \"Firefox\" and \"Navigator\";
lower versions use other classnames such as \"Firefox-bin\". Either choose the
appropriate one, or cover your bases by using instead something like:

> (className =? "Firefox" <||> className =? "Firefox-bin")

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings". -}

-- | If windows that satisfy the query exist, apply the supplied
-- function to them, otherwise run the action given as
-- second parameter.
ifWindows :: Query Bool -> ([Window] -> X ()) -> X () -> X ()
ifWindows qry f el = withWindowSet $ \wins -> do
  matches <- filterM (runQuery qry) $ W.allWindows wins
  case matches of
    [] -> el
    ws -> f ws

-- | The same as ifWindows, but applies a ManageHook to the first match
-- instead and discards the other matches
ifWindow :: Query Bool -> ManageHook -> X () -> X ()
ifWindow qry mh = ifWindows qry (windows . appEndo <=< runQuery mh . head)

{- | 'action' is an executable to be run via 'safeSpawnProg' (of "XMonad.Util.Run") if the Window cannot be found.
   Presumably this executable is the same one that you were looking for.
   Note that this does not go through the shell. If you wish to run an arbitrary IO action
   (such as 'spawn', which will run its String argument through the shell), then you will want to use
   'raiseMaybe' directly. -}
runOrRaise :: String -> Query Bool -> X ()
runOrRaise = raiseMaybe . safeSpawnProg

-- | See 'raiseMaybe'. If the Window can't be found, quietly give up and do nothing.
raise :: Query Bool -> X ()
raise = raiseMaybe $ return ()

{- | 'raiseMaybe' queries all Windows based on a boolean provided by the
   user. Currently, there are 3 such useful booleans defined in
   "XMonad.ManageHook": 'title', 'resource', 'className'. Each one tests based pretty
   much as you would think. ManageHook also defines several operators, the most
   useful of which is (=?). So a useful test might be finding a @Window@ whose
   class is Firefox. Firefox 3 declares the class \"Firefox\", so you'd want to
   pass in a boolean like @(className =? \"Firefox\")@.

   If the boolean returns @True@ on one or more windows, then XMonad will quickly
   make visible the first result. If no @Window@ meets the criteria, then the
   first argument comes into play.

   The first argument is an arbitrary IO function which will be executed if the
   tests fail. This is what enables 'runOrRaise' to use 'raiseMaybe': it simply runs
   the desired program if it isn't found. But you don't have to do that. Maybe
   you want to do nothing if the search fails (the definition of 'raise'), or
   maybe you want to write to a log file, or call some prompt function, or
   something crazy like that. This hook gives you that flexibility. You can do
   some cute things with this hook. Suppose you want to do the same thing for
   Mutt which you just did for Firefox - but Mutt runs inside a terminal window?
   No problem: you search for a terminal window calling itself \"mutt\", and if
   there isn't you run a terminal with a command to run Mutt! Here's an example
   (borrowing 'runInTerm' from "XMonad.Util.Run"):

  > , ((modm, xK_m), raiseMaybe (runInTerm "-title mutt" "mutt") (title =? "mutt"))
-}
raiseMaybe :: X () -> Query Bool -> X ()
raiseMaybe f qry = ifWindow qry raiseHook f

-- | A manage hook that raises the window.
raiseHook :: ManageHook
raiseHook = ask >>= doF . W.focusWindow

-- | See 'runOrRaise' and 'raiseNextMaybe'. Version that allows cycling through matches.
runOrRaiseNext :: String -> Query Bool -> X ()
runOrRaiseNext = raiseNextMaybe . safeSpawnProg

-- | See 'raise' and 'raiseNextMaybe'. Version that allows cycling through matches.
raiseNext :: Query Bool -> X ()
raiseNext = raiseNextMaybe $ return ()

{- | See 'raiseMaybe'.
     'raiseNextMaybe' is an alternative version that allows cycling
     through the matching windows. If the focused window matches the
     query the next matching window is raised. If no matches are found
     the function f is executed.
-}

raiseNextMaybe :: X () -> Query Bool -> X ()
raiseNextMaybe f qry = flip (ifWindows qry) f $ \ws -> do
  foc <- withWindowSet $ return . W.peek
  case foc of
    Just w | w `elem` ws -> let (_:y:_) = dropWhile (/=w) $ cycle ws -- cannot fail to match
                            in windows $ W.focusWindow y
    _ -> windows . W.focusWindow . head $ ws

-- | Given a function which gets us a String, we try to raise a window with that classname,
--   or we then interpret that String as a executable name.
raiseVar :: IO String -> X ()
raiseVar getvar = liftIO getvar >>= \var -> runOrRaise var (fmap (map toLower) className =? var)

{- | 'raiseBrowser' and 'raiseEditor' grab $BROWSER and $EDITOR respectively and they either
     take you to the specified program's window, or they try to run it. This is most useful
     if your variables are simple and look like \"firefox\" or \"emacs\". -}
raiseBrowser, raiseEditor :: X ()
raiseBrowser = raiseVar getBrowser
raiseEditor  = raiseVar getEditor

{- | If the window is found the window is focused and the third argument is called
     otherwise, the first argument is called
     See 'raiseMaster' for an example. -}
raiseAndDo :: X () -> Query Bool -> (Window -> X ()) -> X ()
raiseAndDo f qry after = ifWindow qry (afterRaise `mappend` raiseHook) f
    where afterRaise = ask >>= (>> idHook) . liftX . after

{- | If a window matching the second argument is found, the window is focused and the third argument is called;
     otherwise, the first argument is called. -}
runOrRaiseAndDo :: String -> Query Bool -> (Window -> X ()) -> X ()
runOrRaiseAndDo = raiseAndDo . safeSpawnProg

{- | if the window is found the window is focused and set to master
     otherwise, the first argument is called.

     > raiseMaster (runInTerm "-title ghci"  "zsh -c 'ghci'") (title =? "ghci") -}
raiseMaster :: X () -> Query Bool -> X ()
raiseMaster raisef thatUserQuery = raiseAndDo raisef thatUserQuery (\_ -> windows W.swapMaster)

{- |  If the window is found the window is focused and set to master
      otherwise, action is run.

      > runOrRaiseMaster "firefox" (className =? "Firefox"))
  -}
runOrRaiseMaster :: String -> Query Bool -> X ()
runOrRaiseMaster run query = runOrRaiseAndDo run query (\_ -> windows W.swapMaster)
