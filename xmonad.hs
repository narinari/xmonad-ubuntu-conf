{-
  This is my xmonad configuration file.
  There are many like it, but this one is mine.

  If you want to customize this file, the easiest workflow goes
  something like this:
    1. Make a small change.
    2. Hit "super-q", which recompiles and restarts xmonad
    3. If there is an error, undo your change and hit "super-q" again to
       get to a stable place again.
    4. Repeat

  Author:     David Brewer
  Repository: https://github.com/davidbrewer/xmonad-ubuntu-conf
-}

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import Data.List
import Data.Ratio ((%))
import qualified Data.Map as M
import Data.Maybe
import Data.Traversable(traverse)

import Graphics.X11.Xinerama
import System.IO

import XMonad
import XMonad.Main
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowMenu
import XMonad.Hooks.SetWMName
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.IM
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.Plane
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ICCCMFocus
import qualified XMonad.StackSet as W

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

-- colors
myFocusedBorderColor = "#bd93f9"      -- color of focused border
myNormalBorderColor  = "#cccccc"      -- color of inactive border
myTitleColor     = "#eeeeee"  -- color of window title
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
fg        = "#ebdbb2"
bg        = "#282828"
gray      = "#a89984"
bg1       = "#3c3836"
bg2       = "#505050"
bg3       = "#665c54"
bg4       = "#7c6f64"

green     = "#b8bb26"
darkgreen = "#98971a"
red       = "#fb4934"
darkred   = "#cc241d"
yellow    = "#fabd2f"
blue      = "#83a598"
purple    = "#d3869b"
aqua      = "#8ec07c"
white     = "#eeeeee"

pur2      = "#5b51c9"
blue2     = "#2266d0"

-- Font
myFont = "xft:SpaceMono Nerd Font Mono:" ++ "fontformat=truetype:size=10:antialias=true"

-- props
myModMask            = mod4Mask       -- changes the mod key to "super"
myBorderWidth        = 1              -- width of border around windows
myTerminal           = "termite"   -- which terminal software to use
myIMRosterTitle      = "ハングアウト"   -- title of roster on IM workspace
                                      -- use "Buddy List" for Pidgin, but
                                      -- "Contact List" for Empathy

{-
  Xmobar configuration variables. These settings control the appearance
  of text which xmonad is sending to xmobar via the DynamicLog hook.
-}

myTitleLength    = 80         -- truncate window title to this length
myCurrentWSLeft  = "["        -- wrap active workspace with these
myCurrentWSRight = "]"
myVisibleWSLeft  = "("        -- wrap inactive workspace with these
myVisibleWSRight = ")"
myUrgentWSLeft  = "{"         -- wrap urgent workspace with these
myUrgentWSRight = "}"

hangoutWinClassName = "crx_nckgahadagoaajjgafhacjanaoiihapd"

{-
  Workspace configuration. Here you can change the names of your
  workspaces. Note that they are organized in a grid corresponding
  to the layout of the number pad.

  I would recommend sticking with relatively brief workspace names
  because they are displayed in the xmobar status bar, where space
  can get tight. Also, the workspace labels are referred to elsewhere
  in the configuration file, so when you change a label you will have
  to find places which refer to it and make a change there as well.

  This central organizational concept of this configuration is that
  the workspaces correspond to keys on the number pad, and that they
  are organized in a grid which also matches the layout of the number pad.
  So, I don't recommend changing the number of workspaces unless you are
  prepared to delve into the workspace navigation keybindings section
  as well.
-}

myWorkspaces =
  [
    "7:Chat",  "8:Dbg", "9:Pix",
    "4:Docs",  "5:Dev", "6:Web",
    "1:Term",  "2:Hub", "3:Mail",
    "0:VM",    "Extr1", "Extr2"
  ]

startupWorkspace = "6:Web"  -- which workspace do you want to be on after launch?

{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders
  $ avoidStruts
  $ minimize
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
  $ ResizableTall 1 (3/100) (1/2) []
  ||| ResizableTall 1 (3/100) (3/5) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  -- ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  -- ||| Circle

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid
--  ||| minimize (Tall 1 (3/100) (1/2))))

-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

-- The chat layout uses the "IM" layout. We have a roster which takes
-- up 1/8 of the screen vertically, and the remaining space contains
-- chat windows which are tiled using the grid layout. The roster is
-- identified using the myIMRosterTitle variable, and by default is
-- configured for Pidgin, so if you're using something else you
-- will want to modify that variable.
chatLayout = avoidStruts(withIM (1%7) (Title myIMRosterTitle) Grid)

-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.
gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayouts =
  onWorkspace "7:Chat" chatLayout
  $ onWorkspace "9:Pix" gimpLayout
  $ defaultLayouts


{-
  Custom keybindings. In this section we define a list of relatively
  straightforward keybindings. This would be the clearest place to
  add your own keybindings, or change the keys we have defined
  for certain functions.

  It can be difficult to find a good list of keycodes for use
  in xmonad. I have found this page useful -- just look
  for entries beginning with "xK":

  http://xmonad.org/xmonad-docs/xmonad/doc-index-X.html

  Note that in the example below, the last three entries refer
  to nonstandard keys which do not have names assigned by
  xmonad. That's because they are the volume and mute keys
  on my laptop, a Lenovo W520.

  If you have special keys on your keyboard which you
  want to bind to specific actions, you can use the "xev"
  command-line tool to determine the code for a specific key.
  Launch the command, then type the key in question and watch
  the output.
-}
myKeyBindings = let
    rofi = "rofi -modi combi,system:rofi-system.sh,calc:qalc,run,ssh -combi-modi window,drun -show combi -sidebar-mode"
  in
  [
    ((myModMask .|. shiftMask, xK_space), sendMessage NextLayout)
    , ((myModMask, xK_b), sendMessage ToggleStruts)
    , ((myModMask, xK_a), sendMessage MirrorShrink)
    , ((myModMask, xK_z), sendMessage MirrorExpand)
    , ((myModMask .|. shiftMask, xK_l), spawn "/usr/bin/xset dpms force suspend && slock")
    , ((myModMask, xK_p), spawn rofi)
    , ((myModMask .|. mod1Mask, xK_space), spawn rofi)
    , ((myModMask, xK_u), focusUrgent)
    , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    , ((0, 0x1008FF11), spawn "amixer -q set Master 10%- && paplay /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+ && paplay /usr/share/sounds/freedesktop/stereo/audio-volume-change.oga")
    -- Brightness Keys
    , ((0, 0x1008FF02), spawn "xbacklight + 10")
    , ((0, 0x1008FF03), spawn "xbacklight - 10")
    , ((myModMask, xK_m), withFocused minimizeWindow)
    , ((myModMask .|. shiftMask, xK_m ), withLastMinimized maximizeWindow)
    , ((myModMask, xK_g), goToSelected def)
    , ((myModMask, xK_o), windowMenu)
    , ((shiftMask, 0x1008ff2a), spawn "sudo /sbin/reboot")
    , ((myModMask .|. shiftMask, 0x1008ff2a), spawn "sudo /sbin/shutdown -h now")
  ]


{-
  Management hooks. You can use management hooks to enforce certain
  behaviors when specific programs or windows are launched. This is
  useful if you want certain windows to not be managed by xmonad,
  or sent to a specific workspace, or otherwise handled in a special
  way.

  Each entry within the list of hooks defines a way to identify a
  window (before the arrow), and then how that window should be treated
  (after the arrow).

  To figure out to identify your window, you will need to use a
  command-line tool called "xprop". When you run xprop, your cursor
  will temporarily change to crosshairs; click on the window you
  want to identify. In the output that is printed in your terminal,
  look for a couple of things:
    - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.

  The className values tend to be generic, and might match any window or
  dialog owned by a particular program. The resource values tend to be
  more specific, and will be different for every dialog. Sometimes you
  might want to compare both className and resource, to make sure you
  are matching only a particular window which belongs to a specific
  program.

  Once you've pinpointed the window you want to manipulate, here are
  a few examples of things you might do with that window:
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace. Useful
      for keeping specific tasks on specific workspaces. In the example
      below I have specific workspaces for chat, development, and
      editing images.
-}

-- | Match against @WM_ROLE@.
role :: Query String
role = stringProperty "WM_WINDOW_ROLE"

myManagementHooks :: [ManageHook]
myManagementHooks = [
  isDialog <||> isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_UTILITY" --> doFloat
  , isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODA" --> doFloat
  , resource =? "synapse" --> doIgnore
  , resource =? "stalonetray" --> doIgnore
  , className =? "rdesktop" --> doFloat
  , className =? "Vpnui" --> doFloat
  , className =? "feh" --> doFloat
  , className =? "OnBoard" --> doFloat
  , className =? "MComix" --> doF (W.shift "2:Hub")
  , className =? "virt-manager" --> doF (W.shift "0:VM")
  , (className =? "Code") --> doF (W.shift "5:Dev")
  , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
  , (className =? "Empathy") --> doF (W.shift "7:Chat")
  , (className =? "Pidgin") --> doF (W.shift "7:Chat")
  , (className =? "Gimp-2.8") --> doF (W.shift "9:Pix")
  , (className =? hangoutWinClassName) --> doF (W.shift "7:Chat")
  , (className =? "google-chrome-beta") --> doF (W.shift "6:Web")
  , (className =? "Firefox") --> doF (W.shift "6:Web")
  , (className =? "zeal") --> doFloat
  ]


{-
  Workspace navigation keybindings. This is probably the part of the
  configuration I have spent the most time messing with, but understand
  the least. Be very careful if messing with this section.
-}

-- We define two lists of keycodes for use in the rest of the
-- keyboard configuration. The first is the list of numpad keys,
-- in the order they occur on the keyboard (left to right and
-- top to bottom). The second is the list of number keys, in an
-- order corresponding to the numpad. We will use these to
-- make workspace navigation commands work the same whether you
-- use the numpad or the top-row number keys. And, we also
-- use them to figure out where to go when the user
-- uses the arrow keys.
numPadKeys =
  [
    xK_KP_Home, xK_KP_Up, xK_KP_Page_Up
    , xK_KP_Left, xK_KP_Begin,xK_KP_Right
    , xK_KP_End, xK_KP_Down, xK_KP_Page_Down
    , xK_KP_Insert, xK_KP_Delete, xK_KP_Enter
  ]

numKeys =
  [
    xK_7, xK_8, xK_9
    , xK_4, xK_5, xK_6
    , xK_1, xK_2, xK_3
    , xK_0, xK_minus, xK_equal
  ]

-- Here, some magic occurs that I once grokked but has since
-- fallen out of my head. Essentially what is happening is
-- that we are telling xmonad how to navigate workspaces,
-- how to send windows to different workspaces,
-- and what keys to use to change which monitor is focused.
myKeys = myKeyBindings ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numPadKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  [
    ((m .|. myModMask, k), windows $ f i)
       | (i, k) <- zip myWorkspaces numKeys
       , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++
  M.toList (planeKeys myModMask (Lines 4) Finite) ++
  [
    ((m .|. myModMask, key), screenWorkspace sc
      >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [1,0,2]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
  ]

-------------------- Support for per-screen xmobars ---------
-- Some parts of this should be merged into contrib sometime
getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo

multiPP :: PP -- ^ The PP to use if the screen is focused
        -> PP -- ^ The PP to use otherwise
        -> [Handle] -- ^ Handles for the status bars, in order of increasing X
                    -- screen number
        -> X ()
multiPP = multiPP' dynamicLogString

multiPP' :: (PP -> X String) -> PP -> PP -> [Handle] -> X ()
multiPP' dynlStr focusPP unfocusPP handles = do
    state <- get
    let pickPP :: WorkspaceId -> WriterT (Last XState) X String
        pickPP ws = do
            let isFoc = (ws ==) . W.tag . W.workspace . W.current $ windowset state
            put state {windowset = W.view ws $ windowset state }
            out <- lift $ dynlStr $ if isFoc then focusPP else unfocusPP
            when isFoc $ get >>= tell . Last . Just
            return out
    traverse put . getLast
        =<< execWriterT . (io . zipWithM_ hPutStrLn handles <=< mapM pickPP) . catMaybes
        =<< mapM screenWorkspace (zipWith const [0..] handles)
    return ()
 
mergePPOutputs :: [PP -> X String] -> PP -> X String
mergePPOutputs x pp = fmap (intercalate (ppSep pp)) . sequence . sequence x $ pp

onlyTitle :: PP -> PP
onlyTitle pp = def { ppCurrent = ppCurrent pp
                   , ppHidden = ppHidden pp
                   , ppVisible = ppVisible pp
                   , ppLayout = ppLayout pp
                   , ppTitle = ppTitle pp }
 
-- | Requires a recent addition to xmobar (>0.9.2), otherwise you have to use
-- multiple configuration files, which gets messy
xmobarScreen :: Int -> IO Handle
xmobarScreen = spawnPipe . ("xmobar ~/.xmonad/xmobarrc -x " ++) . show
 
myPP :: PP
myPP = xmobarPP
     { ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
     , ppCurrent = xmobarColor myCurrentWSColor ""
       . wrap myCurrentWSLeft myCurrentWSRight
     , ppVisible = xmobarColor myVisibleWSColor ""
       . wrap myVisibleWSLeft myVisibleWSRight
     , ppUrgent = xmobarColor myUrgentWSColor ""
       . wrap myUrgentWSLeft myUrgentWSRight
     }

{-
  Here we actually stitch together all the configuration settings
  and run xmonad. We also spawn an instance of xmobar and pipe
  content into it via the logHook.
-}

main :: IO ()
main = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

  xmonad
    $ withUrgencyHook NoUrgencyHook
    $ docks
    $ defaultConfig
    { focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNormalBorderColor
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , layoutHook = myLayouts
    , workspaces = myWorkspaces
    , modMask = myModMask
    , handleEventHook = fullscreenEventHook <+> docksEventHook
    , startupHook = do
        setWMName "LG3D"
        windows $ W.greedyView startupWorkspace
        spawn "~/.xmonad/startup-hook"
    , manageHook = manageHook def
        <+> composeAll myManagementHooks
        <+> manageDocks
    , logHook = dynamicLogWithPP(logToDBus dbus)
        <+> takeTopFocus
        <+> updatePointer (0.5, 0.5) (0.2, 0.2)
    } `additionalKeys` myKeys `removeKeys` [(myModMask, xK_space)]

-- Override the PP values as you would otherwise, adding colors etc depending
-- on  the statusbar used
logToDBus :: D.Client -> PP
logToDBus dbus = def
    { ppOutput = dbusOutput dbus
    , ppCurrent = wrap ("%{F" ++ purple ++ "} ") " %{F-}"
    , ppVisible = wrap ("%{F" ++ blue ++ "} ") " %{F-}"
    , ppUrgent = wrap ("%{F" ++ red ++ "} ") " %{F-}"
    , ppHidden = wrap " " " "
    , ppWsSep = ""
    , ppSep = " | "
    , ppTitle = fixWitdh 40
}

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"


fixWitdh :: Int -> String -> String
fixWitdh len str = sstr ++ replicate (len - length sstr) ' '
  where
    sstr = shorten len str

