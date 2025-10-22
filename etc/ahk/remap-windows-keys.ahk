#Requires AutoHotkey v2.0
#SingleInstance Force
#UseHook
 
;; -------------------------------------------------------------
;; Info
;;
;; Target: AutoHotkey v2.0+
;;
;; How to check window class or exe:
;;   Press the Windows key, search for "Window Spy" and run it.
;;
;; How to compile to an executable:
;;   Open ahk2exe.exe from Explorer,
;;   Select this script in the "Source (script file)" field,
;;   Then press the "Convert" button.
;;
;; Modifier keys:
;;   Shift  = +
;;   Ctrl   = ^
;;   Alt    = !
;;   Windows = #
;; -------------------------------------------------------------
 
;; --- Dictionary shortcut -------------------------------------
sc070::Run("C:\Windows\System32\IME\IMEJP\IMJPDCT.exe")
 
;; --- Wheel (Shift + Wheel to scroll horizontally) ------------
+WheelUp::Send("{WheelLeft}")
+WheelDown::Send("{WheelRight}")
 
;; --- Cursor movement -----------------------------------------
#HotIf !WinActive("ahk_exe emacs.exe") && !WinActive("ahk_exe mintty.exe")
;; ^h::Send("{Left}")
^j::Send("{Down}")
^k::Send("{Up}")
;; ^l::Send("{Right}")
#HotIf
 
;; --- Window operation ----------------------------------------
^!w::Send("!{F4}")
 
;; --- Emacs Evil mode ESC handling (ESC or Ctrl-[) ------------
ESC::Send("{ESC}{vk1Dsc07B}")
^[::Send("{ESC}{vk1Dsc07B}")
 
;; --- Emacs in terminal (mintty) special key mappings ---------
#HotIf WinActive("ahk_exe mintty.exe")
^vkBA::Send("!{vkBA}")   ; Ctrl+: → Alt+:
^Enter::Send("!x")       ; Ctrl+Enter → Alt+x
^.::Send("!o")           ; Ctrl+. → Alt+o
#HotIf
 
;; --- Quake ---------------------------------------------------
^Space:: {
  win := "ahk_exe emacs.exe"
  if WinExist(win) {
    if WinActive(win) {
      WinMoveBottom(win)
      Send("!{Tab}")
    } else {
      WinActivate(win)
    }
  } else {
    Run("runemacs.exe")
  }
}

#@:: {
  win := "ahk_exe emacs.exe"
 
  if !WinExist(win) {
    Run("runemacs.exe")
  } else {
    mm := WinGetMinMax(win)   ; -1:min, 0:norm, 1:max
 
    if (mm = -1) {
      WinRestore(win)
      WinMaximize(win)
      WinActivate(win)
    } else if (mm = 0) {
      WinMaximize(win)
      WinActivate(win)
    } else {
      if WinActive(win) {
        WinMinimize(win)
      } else {
        WinMaximize(win)
        WinActivate(win)
      }
    }
  }
}