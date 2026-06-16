$lnkDir = "$env:USERPROFILE\.local\bin"
$msys2 = "$env:USERPROFILE\scoop\apps\msys2\current"
$mintty = "$msys2\usr\bin\mintty.exe"
$ws = New-Object -ComObject WScript.Shell
 
$s = $ws.CreateShortcut("$lnkDir\ucrt64-zsh.lnk")
$s.TargetPath = $mintty
$s.Arguments = "--store-taskbar-properties -e /usr/bin/env MSYSTEM=UCRT64 MSYS2_PATH_TYPE=inherit /usr/bin/zsh --login -i"
$s.WorkingDirectory = $env:USERPROFILE
$s.IconLocation = "$msys2\ucrt64.ico"
$s.Save()
