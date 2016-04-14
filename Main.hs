{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (when)
import Graphics.UI.Gtk hiding (eventKeyName, eventModifier, eventButton, eventTime)
import Graphics.UI.Gtk.Vte.Vte
import Graphics.UI.Gtk.Gdk.Events
import Data.Foldable (traverse_)
import Data.Text (Text)

shortcut :: [Modifier] -> Text -> Event -> Bool
shortcut mods name (Key { eventModifier = mods', eventKeyName = name' }) =
  all (`elem` mods') mods && name == name'
shortcut _ _ _ =
  False

movePage :: NotebookClass notebook => notebook -> (Int -> Int) -> IO ()
movePage notebook f = do
  i <- notebookGetCurrentPage notebook
  p <- notebookGetNthPage notebook i
  traverse_ (flip (notebookReorderChild notebook) (f i)) p

terminalPage :: (NotebookClass notebook, WindowClass window) => window -> notebook -> IO ()
terminalPage window notebook = do
  scrolled <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrolled PolicyAutomatic PolicyAutomatic

  vte <- terminalNew
  _ <- terminalForkCommand vte Nothing (Nothing :: Maybe [String]) Nothing Nothing False False False
  font <- fontDescriptionFromString ("DejaVu Sans Mono 10" :: String)
  terminalSetFont vte font
  terminalSetScrollbackLines vte 10000

  containerAdd scrolled vte

  page <- notebookAppendPage notebook scrolled ("" :: String)

  _ <- on vte windowTitleChanged $ do
    title <- terminalGetWindowTitle vte :: IO String
    notebookSetTabLabelText notebook scrolled (title :: String)
  _ <- on vte childExited $ get notebook notebookCurrentPage >>= notebookRemovePage notebook

  _ <- on notebook pageRemoved $ \_ _ -> do
    n <- notebookGetNPages notebook
    when (n == 0) mainQuit

  let f e | shortcut [Shift, Control] "V" e = do
        terminalPasteClipboard vte
        return True
      f e | shortcut [Shift, Control] "C" e = do
        terminalCopyClipboard vte
        return True
      f _ = return False

  _ <- onKeyPress vte f

  let p (Button { eventButton = eb@RightButton, eventTime = et }) = do
        menu <- menuNew

        copyItem <- menuItemNewWithLabel ("Copy" :: String)
        _ <- onActivateLeaf copyItem $ terminalCopyClipboard vte
        menuShellAppend menu copyItem
        widgetShow copyItem

        pasteItem <- menuItemNewWithLabel ("Paste" :: String)
        _ <- onActivateLeaf pasteItem $ terminalPasteClipboard vte
        menuShellAppend menu pasteItem
        widgetShow pasteItem

        menuPopup menu (Just (eb, et))

        return True
      p _ = return False

  _ <- onButtonPress vte p

  widgetShowAll window
  notebookSetCurrentPage notebook page
  widgetGrabFocus vte

main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  _ <- onDestroy window mainQuit
  widgetSetSizeRequest window 640 480

  notebook <- notebookNew
  set notebook [notebookScrollable := True]
  notebookSetHomogeneousTabs notebook True
  containerAdd window notebook

  terminalPage window notebook

  let f e | shortcut [Control, Shift] "T" e = do
        terminalPage window notebook
        return True
      f e | shortcut [Shift, Control] "Home" e = do
        movePage notebook $ const 0
        return True
      f e | shortcut [Shift, Control] "End" e = do
        movePage notebook $ const (-1)
        return True
      f e | shortcut [Control] "Home" e = do
        notebookSetCurrentPage notebook 0
        return True
      f e | shortcut [Control] "End" e = do
        notebookSetCurrentPage notebook (-1)
        return True
      f e | shortcut [Shift, Control] "Page_Up" e = do
        movePage notebook pred
        return True
      f e | shortcut [Shift, Control] "Page_Down" e = do
        movePage notebook succ
        return True
      f e | shortcut [Control] "Page_Up" e = do
        notebookPrevPage notebook
        return True
      f e | shortcut [Control] "Page_Down" e = do
        notebookNextPage notebook
        return True
      f _ = return False

  _ <- onKeyPress window f

  mainGUI
