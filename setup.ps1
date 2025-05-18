#Requires -Version 5.0
<#
.SYNOPSIS
    Setup environment for Windows using Scoop and symbolic links.
.DESCRIPTION
    This script links dotfiles and installs essential apps with Scoop.
#>

# --- Functions ---------------------------------------------------------------

function New-SafeLink {
  param(
    [string[]]$Sources,
    [string]$TargetDir
  )
  foreach ($src in $Sources) {
    if (Test-Path $src) {
      $base = Split-Path $src -Leaf
      if ($base -notin '.', '..', '.DS_Store') {
        $dest = Join-Path $TargetDir $base
        if (Test-Path $dest) {
          Write-Host "🔁 Replacing existing item: $dest"
          Remove-Item -Force -Recurse $dest
        }
        Write-Host "✅ Linking: $dest → $src"
        New-Item -ItemType SymbolicLink -Path $dest -Target $src | Out-Null
      }
    }
  }
}

function New-ContentsLink {
  param(
    [string]$SourceDir,
    [string]$TargetDir
  )
  if (-not (Test-Path $TargetDir)) {
    New-Item -ItemType Directory -Path $TargetDir -Force | Out-Null
  }
  $items = Get-ChildItem -Path $SourceDir -Force
  New-SafeLink -Sources ($items.FullName) -TargetDir $TargetDir
}

# --- Setup Links -------------------------------------------------------------

$dotfiles = "$HOME\dotfiles"

New-ContentsLink "$dotfiles\home" "$HOME"
New-ContentsLink "$dotfiles\config" "$HOME\.config"
New-ContentsLink "$dotfiles\local" "$HOME\.local"
New-ContentsLink "$dotfiles\emacs.d" "$HOME\.emacs.d"

# --- Install Scoop and Core Applications -------------------------------------

Set-ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
iwr -useb get.scoop.sh | iex

$apps = @(
  "git", "7zip", "msys2", "ripgrep", "wsltty",
  "emacs", "espanso", "winmerge", "autohotkey", "draw.io",
  "everything", "Flow-Launcher", "sbcl", "r", "python", "poetry"
)

if (-not (scoop bucket list | Select-String -Pattern "^extras$")) {
  scoop bucket add extras
}

foreach ($app in $apps) {
  scoop install $app
}

# --- Configure wsltty --------------------------------------------------------

$wslttyConfigSource = "$PSScriptRoot\..\etc\windows-wsltty\config"
$wslttyConfigDest = "$env:USERPROFILE\scoop\persist\wsltty\config\config"

Write-Host "🔧 Copying wsltty config..."
Copy-Item -Force $wslttyConfigSource $wslttyConfigDest

# --- Done --------------------------------------------------------------------

Write-Host "`n🎉 Setup complete!"
