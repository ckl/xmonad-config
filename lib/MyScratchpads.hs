---------------------------------------------------------------------
-- |
-- Module     : MyScratchpads.hs
--
-- This module is similar to XMonad.Util.NamedScratchpad and also
-- draws inspiration from pbrisbin's ScratchPadKeys. Since Gnome-terminal
-- seems incompatible with runInTerminal (does not support '-name'),
-- xterm is used by default. Other additions are more manage hooks
-- for specifying location, which take width and height parameters,
-- instead of just a height.
---------------------------------------------------------------------
module MyScratchpads 
    ( Scratchpad
    , makeScratchpad
    , makeTerminalScratchpad
    , getScratchpadKeys
    , getScratchpadHooks
      -- Manage hooks
    , centerScreen
    , centerRight
    , bottomLeft
    , bottomCenter
    , bottomRight
    , bottomEdge
    , topLeft
    , topCenter
    , topRight
    ) where

import XMonad
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Hooks.ManageHelpers       (doRectFloat)
import XMonad.Util.Run
import XMonad.Util.Scratchpad

import Control.Arrow                    ((&&&))
import Control.Monad                    (filterM, unless)

import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- $usage
-- In your xmonad.hs:
-- 
-- > import MyScratchpads
--
-- Then create a list of your scratchpads:
--
-- > myScratchpads :: [Scratchpad]
-- > myScratchpads =  [cmus, htop]
--
-- Now define what each of your scratchpads should be. A definition
-- includes whether it's a terminal app or not, the key to bind it to,
-- the command to run, and the manage hook (where it appears on screen).
-- Examples:
--
-- > cmus :: Scratchpad
-- > cmus = makeTerminalScratchpad (mod4Mask, xK_m) "cmus" $ centerScreen 0.5 0.5
--
-- Here's how to make scratchpads for windowed applications
-- (Make a scratchpad for lxappearance):
-- > lxappearance:: Scratchpad
-- > lxappearance = makeScratchpad (mod4Mask, xK_b)      -- keybinding
-- >                               "lxappearance"        -- command name
-- >                               "Lxappearance" $      -- class name (from xprop)
-- >                               centerScreen 0.5 0.8  -- location hook
--
-- Then combine them with your manage hook:
--
-- > myManageHook = manageHook defaultConfig <+>
--                  ... <+> getScratchpadHooks myScratchpads
--
-- And don't forget to bind your keys:
--
-- > myKeys = M.fromList $
-- >          [ ... 
-- >            ...
-- >          ] ++ getScratchpadKeys myScratchpads
--
-- Check out my xmonad.hs for more information.


-- | Scratchpad data type.
data Scratchpad = Scratchpad 
    { key   :: (KeyMask, KeySym)    -- keybinding to be used
    , cmd   :: X ()                 -- command to run
    , query :: Query Bool           -- query to find it once it's spawned
    , hook  :: ManageHook           -- location of the window on screen
    }

-- Default terminal used to execute the command. My default terminal,
-- gnome-terminal, doesn't work (doesn't support '-name' switch), so 
-- use xterm with these defaults.
term :: String
term = "xterm -bg black -fg white -fa dejavu -fs 10 "

-- | mkTermScratchPad : Creates the terminal scratchpad with the specified
--   keybinding and command to be run. Query is "sp-<command>", and hook is
--   one of the window location hooks defined at the bottom.
makeTerminalScratchpad :: (KeyMask, KeySym) -> String -> ManageHook -> Scratchpad
makeTerminalScratchpad k c h = Scratchpad
    { key   = k
    , cmd   = runInTerminal ["-name", "sp-" ++ c, "-e", c]
    , query = resource =? ("sp-" ++ c)
    , hook  = h
    }

-- | makeScratchpad : Creates a scratchpad for the specified window 
--   application. Similar to makeTerminalScratchpad above, except you
--   MUST provide the class name to query for the application. You can
--   find this by typing 'xprop | grep WM_CLASS' and clicking on the 
--   target application. It's usually the second parameter in the list.
makeScratchpad :: (KeyMask, KeySym) -> String -> String -> ManageHook -> Scratchpad
makeScratchpad k c q h = Scratchpad
    { key   = k
    , cmd   = spawn c
    , query = className =? q
    , hook  = h
    }

-- | runInTerminal : takes arguments of the command in the form of
--   a list and returns an X window object, namely, the spawned terminal.
runInTerminal :: [String] -> X ()
runInTerminal args = spawn $ term ++ (unwords args) 

-- | getScratchpadKeys : Takes a list of terminal scratchpads as input
--   (defined above) and returns a list of tuples of keybindings that
--   are to be appended to the keybindings used in xmonad.hs.
getScratchpadKeys :: [Scratchpad] -> [((KeyMask, KeySym), X ())]
getScratchpadKeys = fmap (key &&& spawnScratchpad)

-- | getScratchpadHooks : Takes a list of scratchpads and returns a
--   manage hook that combines all of them. Combine the returned result
--   with your manage hook in your xmonad.hs.
getScratchpadHooks :: [Scratchpad] -> ManageHook
getScratchpadHooks = composeAll . fmap (\c -> query c --> hook c)

-- | spawnScratchpad : Summon, banish, or spawn a single scratchpad.
--   (Borrowed and adapted from XMonad.Util.NamedScratchpad.hs)
spawnScratchpad :: Scratchpad -> X ()
spawnScratchpad sp = withWindowSet $ \s -> do
    -- Search for the window on the current workspace
    filterCurrent <- filterM (runQuery $ query sp) .
        maybe [] W.integrate . W.stack . W.workspace $ W.current s

    case filterCurrent of
        -- The window was not found on the current workspace
        [] -> do
            -- Search all workspaces for the window
            filterAll <- filterM (runQuery $ query sp) $ W.allWindows s
            -- If it's not found, then run the command. If it is found
            -- then move the window to the current workspace
            case filterAll of
                []    -> cmd sp
                (x:_) -> windows $ W.shiftWin (W.currentTag s) x
        -- The window was found on the current workspace
        (x:_) -> do
            -- Add the hidden workspace "NSP" if it doesn't already exist
            unless
                (any ((== "NSP") . W.tag) $ W.workspaces s) $
                addHiddenWorkspace "NSP"
            -- Move the window to the hidden workspace
            windows $ W.shiftWin "NSP" x


--------------------------------------------------------------------------
-- ManageHooks
--
-- Each of these functions takes a width and/or a height, then returns
-- a manage hook for the specified screen position. 0.99 is used
-- instead of 1.0 for determining position to leave a small gap
-- between the border of the screen and the window.
--
-- Note: parameters for W.RationalRect are l t w h where
--       l = distance from the left as a percent
--       t = distance from the top as a percent
--       w = width as a percent
--       h = height as a percent

-- | Floating rectangular window at the bottom left
bottomLeft :: Rational -> Rational -> ManageHook
bottomLeft w h = doRectFloat $ W.RationalRect 0.01 (0.99 - h) w h

-- | Floats window along the bottom edge and centers it
bottomCenter :: Rational -> Rational -> ManageHook
bottomCenter w h = doRectFloat $ W.RationalRect ((1 - w)/2) (1 - h) w h

-- | Floating rectangular window at the bottom right
bottomRight :: Rational -> Rational -> ManageHook
bottomRight w h = doRectFloat $ W.RationalRect (0.99 - w) (0.99 - h) w h

-- | Floating rectangular window at the top left 
topLeft :: Rational -> Rational -> ManageHook
topLeft w h = doRectFloat $ W.RationalRect 0.01 0.01 w h

-- | Floats a rectangular window at the top center
topCenter :: Rational -> Rational -> ManageHook
topCenter w h = doRectFloat $ W.RationalRect ((1 - w)/2) 0.01 w h

-- | Floating rectangular window at the top right
topRight :: Rational -> Rational -> ManageHook
topRight w h = doRectFloat $ W.RationalRect (0.99 - w) 0.01 w h

centerRight :: Rational -> Rational -> ManageHook
centerRight w h = doRectFloat $ W.RationalRect (0.99 - w) ((1 - h)/2) w h

-- | Floating rectangular window at the center of the screen
centerScreen :: Rational -> Rational -> ManageHook
centerScreen w h = doRectFloat $ W.RationalRect ((1 - w)/2) ((1 - h)/2) w h

-- | Floats along the entire bottom edge of the screen with the specified height
bottomEdge :: Rational -> ManageHook
bottomEdge h = doRectFloat $ W.RationalRect 0 (1 - h) 1 h


