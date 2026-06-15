$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$msys2 = "$env:USERPROFILE\scoop\apps\msys2\current"
$ws = New-Object -ComObject WScript.Shell
 
$s = $ws.CreateShortcut("$scriptDir\msys2-zsh.lnk")
$s.TargetPath = "$msys2\msys2_shell.cmd"
$s.Arguments = "-mintty -shell zsh"
$s.IconLocation = "$msys2\msys2.ico"
$s.Save()
 
$s = $ws.CreateShortcut("$scriptDir\ucrt64-zsh.lnk")
$s.TargetPath = "$msys2\msys2_shell.cmd"
$s.Arguments = "-mintty -ucrt64 -shell zsh"
$s.IconLocation = "$msys2\ucrt64.ico"
$s.Save()
