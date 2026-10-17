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
 
;; --- org-moor: take the document in front (Ctrl+Alt+M) --------
;; Runs once and exits.  The window is read here, before PowerShell starts,
;; so the hidden console never stands in front of what is being taken.
^!m:: {
    hwnd := WinExist("A")
    script := EnvGet("USERPROFILE") "\.emacs.d\straight\repos\org-moor\script\org-moor-front.ps1"
    Run('powershell.exe -NoProfile -ExecutionPolicy Bypass -WindowStyle Hidden -File "' script '" -Hwnd ' hwnd, , "Hide")
}
 
;; --- Wheel (Shift + Wheel to scroll horizontally) ------------
+WheelUp::Send("{WheelLeft}")
+WheelDown::Send("{WheelRight}")
 
;; --- Cursor movement -----------------------------------------
#HotIf !WinActive("ahk_exe emacs.exe") && !WinActive("ahk_exe mintty.exe")
^j::Send("{Down}")
^k::Send("{Up}")
#HotIf
 
;; --- Emacs Evil mode ESC handling (ESC or Ctrl-[) ------------
ESC::Send("{ESC}{vk1Dsc07B}")
^[::Send("{ESC}{vk1Dsc07B}")
 
