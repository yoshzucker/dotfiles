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

;; --- Claude text transform (Ctrl+Alt+C) ----------------------
;; Select text in any app, press Ctrl+Alt+C, pick a preset from the list
;; (navigate with Ctrl+J / Ctrl+K or the arrow keys, Enter to choose, Esc to
;; cancel). The selection is transformed by the `claude` CLI (via
;; claude-clip.ps1) and pasted back over the selection. The original clipboard
;; is restored afterward.
;;
;; A custom Gui is used instead of a native Menu on purpose: native popup menus
;; are modal and block AHK hotkeys, so Ctrl+J/K cannot drive them. A Gui is a
;; normal window, so the existing ^j/^k -> Down/Up bindings navigate its list.
^!c:: {
  ClaudeTransform()
}

ClaudeTransform() {
  savedClip := ClipboardAll()            ; full-fidelity original
  A_Clipboard := ""
  Send("^c")
  if !ClipWait(1) {                      ; no selection
    A_Clipboard := savedClip
    ToolTip("選択テキストがありません")
    SetTimer(() => ToolTip(), -1200)
    return
  }
  preset := ClaudeChoosePreset()         ; blocks until a choice or cancel
  if (preset = "") {                     ; cancelled / no presets -> leave as-is
    A_Clipboard := savedClip
    return
  }
  RunTransform(preset, savedClip)
  A_Clipboard := savedClip               ; restore original
}

;; Show the preset chooser centered on the active window. The menu items are the
;; *.md files found in ~/.config/claude/prompts (files whose name starts with
;; "_", e.g. _guard.md, are skipped). The label is the file name without .md.
;; Returns the chosen preset name, or "" if cancelled / none available.
ClaudeChoosePreset() {
  promptsDir := EnvGet("USERPROFILE") "\.config\claude\prompts"
  names := []
  Loop Files, promptsDir "\*.md" {
    if (SubStr(A_LoopFileName, 1, 1) = "_")
      continue
    names.Push(SubStr(A_LoopFileName, 1, -3))   ; strip ".md"
  }
  if (names.Length = 0) {
    ToolTip("プロンプトがありません: " promptsDir)
    SetTimer(() => ToolTip(), -2000)
    return ""
  }
  ; Sort alphabetically for a stable menu order.
  loop names.Length {
    i := A_Index
    loop names.Length - i {
      j := A_Index
      if (StrCompare(names[j], names[j + 1]) > 0) {
        tmp := names[j], names[j] := names[j + 1], names[j + 1] := tmp
      }
    }
  }
  res := { choice: "" }

  ; Capture the active (source) window's rect before the Gui steals focus.
  WinGetPos(&wx, &wy, &ww, &wh, "A")

  cg := Gui("+AlwaysOnTop -MinimizeBox -MaximizeBox", "Claude変換")
  cg.SetFont("s11")
  lb := cg.AddListBox("w240 r" names.Length, names)
  lb.Choose(1)
  btn := cg.AddButton("Default w240", "変換 (Enter / Ctrl+Enter)")

  pick(*) {
    v := lb.Value
    if (v >= 1)
      res.choice := names[v]
    cg.Destroy()
  }
  cancel(*) => cg.Destroy()

  lb.OnEvent("DoubleClick", pick)
  btn.OnEvent("Click", pick)
  cg.OnEvent("Escape", cancel)
  cg.OnEvent("Close", cancel)

  cg.Show("Hide")                        ; realize to measure, then center on source window
  cg.GetPos(, , &gw, &gh)
  px := wx + (ww - gw) // 2
  py := wy + (wh - gh) // 2
  cg.Show("x" px " y" py)
  lb.Focus()                             ; so Ctrl+J/K and arrows move the selection

  WinWaitClose("ahk_id " cg.Hwnd)
  return res.choice
}

RunTransform(preset, savedClip) {
  ToolTip("変換中… (" preset ")")
  psPath := EnvGet("USERPROFILE") "\.local\bin\claude-clip.ps1"
  ; Pass the chosen name via an environment variable, not the command line, so
  ; non-ASCII / spaced file names survive intact (the child inherits it).
  EnvSet("CLAUDE_CLIP_PRESET", preset)
  cmd := 'powershell.exe -NoProfile -WindowStyle Hidden -ExecutionPolicy Bypass -File "' psPath '"'
  exitCode := RunWait(cmd, , "Hide")
  ToolTip()
  if (exitCode = 0) {
    Send("^v")
    Sleep(150)
  } else {
    ToolTip("変換に失敗しました")
    SetTimer(() => ToolTip(), -1500)
  }
  Sleep(150)
  A_Clipboard := savedClip               ; restore original
}

;; In the transform chooser, Ctrl+Enter confirms too (same as Enter / the Default
;; button). Scoped to the chooser window so it never affects other apps.
#HotIf WinActive("Claude変換 ahk_class AutoHotkeyGUI")
^Enter::Send("{Enter}")
#HotIf