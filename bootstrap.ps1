#Requires -Version 5.0
<#
.SYNOPSIS
    Windows bootstrap script for the dotfiles repository.
.DESCRIPTION
    PowerShell equivalent of the Unix 'bootstrap' script.
    Designed to have similar command surface and behavior.

    Supported commands:
      (no argument)   Safe link refresh (default)
      link            Create/refresh managed symlinks
      unlink          Remove only symlinks managed by this repo
      update          Install/update Scoop packages + refresh links
      doctor [--fix]  Find (and optionally remove) broken symlinks

    The goal is to stay as close as possible to the bash implementation
    in terms of function names, structure, and safety guarantees.
#>

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition

# =============================================================================
# Core Link Utilities (modeled after bash version)
# =============================================================================

function Ensure-RealDirectory {
    param([string]$Path)
    if (Test-Path $Path -PathType Leaf) {
        Write-Host "🔁 Replacing file with directory: $Path"
        Remove-Item -Force $Path
    }
    if (Test-Path $Path -PathType Leaf) {
        # Should not happen after above, but safety
        Remove-Item -Force $Path
    }
    New-Item -ItemType Directory -Path $Path -Force | Out-Null
}

function Link-DirectoryContents {
    param(
        [string]$SourceDir,
        [string]$TargetDir
    )
    Ensure-RealDirectory $TargetDir

    Get-ChildItem -Path $SourceDir -Force | ForEach-Object {
        $name = $_.Name
        if ($name -in @('.', '..', '.DS_Store')) { return }

        $dest = Join-Path $TargetDir $name

        if (Test-Path $dest) {
            if ((Get-Item $dest).LinkType -eq 'SymbolicLink') {
                Write-Host "🔁 Replacing symlink: $dest"
                Remove-Item -Force $dest
            } else {
                Backup-ConflictingItem $dest
            }
        }

        try {
            New-Item -ItemType SymbolicLink -Path $dest -Target $_.FullName -Force -ErrorAction Stop | Out-Null
            Write-Host "✅ Linked: $dest → $($_.FullName)"
        } catch {
            Write-Host "⚠️  Failed to create symlink: $dest → $($_.FullName)"
            Write-Host "   (Windows may require Developer Mode or Admin rights for symlinks)"
        }
    }
}

function Link-Tree {
    param(
        [string]$SourceDir,
        [string]$TargetDir
    )
    Ensure-RealDirectory $TargetDir

    Get-ChildItem -Path $SourceDir -Force | ForEach-Object {
        $name = $_.Name
        if ($name -in @('.', '..', '.DS_Store')) { return }

        $dest = Join-Path $TargetDir $name

        if ($_.PSIsContainer) {
            Link-Tree $_.FullName $dest
        } else {
            if (Test-Path $dest) {
                if ((Get-Item $dest).LinkType -eq 'SymbolicLink') {
                    Remove-Item -Force $dest
                } else {
                    Backup-ConflictingItem $dest
                }
            }

            try {
                New-Item -ItemType SymbolicLink -Path $dest -Target $_.FullName -Force -ErrorAction Stop | Out-Null
                Write-Host "✅ Linked: $dest → $($_.FullName)"
            } catch {
                Write-Host "⚠️  Failed to create symlink: $dest → $($_.FullName)"
                Write-Host "   (Windows may require Developer Mode or Admin rights for symlinks)"
            }
        }
    }
}

function Backup-ConflictingItem {
    param([string]$Path)
    $timestamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $backupRoot = Join-Path $env:LOCALAPPDATA "dotfiles\backups\$timestamp"
    $relative = $Path.Replace($HOME, "").TrimStart('\')
    $backupPath = Join-Path $backupRoot $relative

    New-Item -ItemType Directory -Path (Split-Path $backupPath) -Force | Out-Null
    Move-Item -Force $Path $backupPath

    Write-Host "⚠️  Conflict: $Path was a real item."
    Write-Host "    Backed up to: $backupPath"
}

function Is-ManagedSymlink {
    param([string]$Path)
    # Use -Force to handle dangling symlinks (target missing); Test-Path would fail for them.
    $item = Get-Item -Path $Path -Force -ErrorAction SilentlyContinue
    if (-not $item) { return $false }
    if ($item.LinkType -ne 'SymbolicLink') { return $false }

    $target = $item.Target
    # Target may be relative or absolute; check if it points inside our repo dir.
    if ($target) {
        try {
            $resolved = [System.IO.Path]::GetFullPath( (Join-Path (Split-Path $Path) $target) ) 2>$null
        } catch {
            $resolved = $target
        }
        if ($resolved -and $resolved.StartsWith($ScriptDir)) { return $true }
        # Also accept if the raw Target string (common for abs symlinks) starts with repo
        if ($target.StartsWith($ScriptDir)) { return $true }
    }
    return $false
}

function Remove-ManagedSymlinksUnder {
    param([string]$BaseDir)
    if (-not (Test-Path $BaseDir)) { return }

    Get-ChildItem -Path $BaseDir -Recurse -Force -ErrorAction SilentlyContinue |
        Where-Object { $_.LinkType -eq 'SymbolicLink' -and (Is-ManagedSymlink $_.FullName) } |
        ForEach-Object {
            Write-Host "🗑️  Removing managed symlink: $($_.FullName)"
            Remove-Item -Force $_.FullName
        }
}

# =============================================================================
# High-level commands (matching bash names)
# =============================================================================

function Setup-Links {
    <#
    .SYNOPSIS
        Creates or refreshes all managed symlinks.
        Mirrors the bash `setup_links` function.
    #>
    Write-Host "Linking dotfiles..."

    Remove-ManagedSymlinksUnder $HOME
    Remove-ManagedSymlinksUnder (Join-Path $HOME ".config")
    Remove-ManagedSymlinksUnder (Join-Path $HOME ".local")

    Link-Tree (Join-Path $ScriptDir "home") $HOME
    Link-Tree (Join-Path $ScriptDir "config") (Join-Path $HOME ".config")

    Ensure-RealDirectory (Join-Path $HOME ".local")
    Ensure-RealDirectory (Join-Path $HOME ".local\bin")
    Link-DirectoryContents (Join-Path $ScriptDir "local\bin") (Join-Path $HOME ".local\bin")

    Write-Host "Linking finished."
    Write-Host "Tip: Run '.\bootstrap.ps1 doctor' to check for any broken symlinks."
}

function Unlink-Dotfiles {
    <#
    .SYNOPSIS
        Removes only symlinks that point back into this dotfiles repository.
        Mirrors the bash `unlink_dotfiles` function.
    #>
    Write-Host "Unlinking managed dotfiles symlinks..."

    Remove-ManagedSymlinksUnder $HOME
    Remove-ManagedSymlinksUnder (Join-Path $HOME ".config")
    Remove-ManagedSymlinksUnder (Join-Path $HOME ".local")

    Write-Host "Unlink finished."
    Write-Host "All managed symlinks have been removed (real files were left untouched)."
}

function Is-BrokenSymlink {
    <#
    .SYNOPSIS
        Returns true if the path is a symlink whose target no longer exists.
    #>
    param([string]$Path)
    # -Force to detect dangling symlinks (Test-Path fails for them)
    $item = Get-Item -Path $Path -Force -ErrorAction SilentlyContinue
    if (-not $item) { return $false }
    if ($item.LinkType -ne 'SymbolicLink') { return $false }
    $target = $item.Target
    if (-not $target) { return $true }
    # For relative targets, resolve against parent dir of the link
    $candidate = $target
    if (-not [System.IO.Path]::IsPathRooted($target)) {
        $candidate = Join-Path (Split-Path $Path) $target
    }
    return -not (Test-Path $candidate)
}

function Doctor {
    <#
    .SYNOPSIS
        Scans for broken (dangling) symlinks under $HOME, ~/.config, ~/.local.
        With -Fix, removes them.
        Managed symlinks whose targets were removed from the repo are also cleaned
        by Setup-Links / Unlink-Dotfiles.
    #>
    param([switch]$Fix)

    Write-Host "Running doctor..."

    $dirs = @($HOME, (Join-Path $HOME ".config"), (Join-Path $HOME ".local"))

    $totalBroken = 0

    foreach ($dir in $dirs) {
        Write-Host "=== $dir ==="
        $broken = Get-ChildItem -Path $dir -Recurse -Force -ErrorAction SilentlyContinue |
            Where-Object { Is-BrokenSymlink $_.FullName }

        if ($Fix) {
            foreach ($link in $broken) {
                Write-Host "🗑️  Removing broken symlink: $($link.FullName)"
                Remove-Item -Force $link.FullName
                $totalBroken++
            }
        } else {
            foreach ($link in $broken) {
                Write-Host "Broken: $($link.FullName)"
                $totalBroken++
            }
        }
    }

    if ($Fix) {
        Write-Host "Removed $totalBroken broken symlink(s)."
    } else {
        Write-Host "Found $totalBroken broken symlink(s)."
        if ($totalBroken -gt 0) {
            Write-Host "Tip: Run '.\bootstrap.ps1 doctor -Fix' to remove them."
        }
    }
}

# =============================================================================
# Package management (Scoop)
# =============================================================================

function Install-Scoop {
    <#
    .SYNOPSIS
        Ensures Scoop is installed.
        Mirrors the bash `install_brew` logic for the Windows side.
    #>
    if (Get-Command scoop -ErrorAction SilentlyContinue) {
        return
    }

    Write-Host "Scoop not found. Installing Scoop..."
    Set-ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
    try {
        iwr -useb get.scoop.sh | iex
    } catch {
        Write-Host "Failed to install Scoop. Please install it manually from https://scoop.sh" -ForegroundColor Yellow
        exit 1
    }

    # Refresh PATH so that 'scoop' is immediately available in this session
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path", "User")

    Write-Host "Scoop installed successfully."
    Write-Host "IMPORTANT: Close and reopen your terminal (or restart PowerShell) for Scoop to work properly in new sessions."
    Write-Host "Then re-run: .\bootstrap.ps1"
    exit 0
}

function Install-ScoopPackages {
    <#
    .SYNOPSIS
        Installs packages via Scoop.
        Strongly prefers pkg/scoop/scoopfile.json if present (declarative, matching pkg/brew/Brewfile).
        Falls back to a small curated list only if the scoopfile is missing.
    #>
    # Ensure minimum required buckets
    $requiredBuckets = @("extras")
    foreach ($b in $requiredBuckets) {
        if (-not (scoop bucket list | Select-String "^$b$")) {
            Write-Host "Adding required Scoop bucket: $b"
            scoop bucket add $b | Out-Null
        }
    }

    $scoopfile = Join-Path $ScriptDir "pkg" "scoop" "scoopfile.json"

    if (Test-Path $scoopfile) {
        Write-Host "Installing from scoopfile..."
        scoop import $scoopfile
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Warning: Some packages from the scoopfile may have failed." -ForegroundColor Yellow
        }
    } else {
        Write-Host "No scoopfile found. Using fallback list."
        Write-Host "Create pkg/scoop/scoopfile.json (via 'scoop export') for declarative management."
        $packages = @("git", "ripgrep", "fzf", "emacs", "r", "python", "sbcl")

        $installed = @()
        $skipped = @()
        $failed = @()

        foreach ($pkg in $packages) {
            if (-not (scoop list | Select-String "^$pkg\s")) {
                scoop install $pkg | Out-Null
                if ($LASTEXITCODE -eq 0) {
                    $installed += $pkg
                } else {
                    $failed += $pkg
                }
            } else {
                $skipped += $pkg
            }
        }

        if ($installed.Count -gt 0) { Write-Host "Installed: $($installed -join ', ')" }
        if ($skipped.Count -gt 0)  { Write-Host "Skipped (already present): $($skipped -join ', ')" }
        if ($failed.Count -gt 0)   { Write-Host "Failed: $($failed -join ', ')" -ForegroundColor Yellow }
    }

    Write-Host "Scoop packages step completed."
}

# =============================================================================
# Main entry point (modeled after bash version)
# =============================================================================

function Show-Help {
    Write-Host @"
bootstrap.ps1 — Windows bootstrap script

Usage:
  .\bootstrap.ps1 [bootstrap]     # Full bootstrap / re-deploy (Scoop + packages + links; recommended for fresh)
  .\bootstrap.ps1 update          # Update Scoop + packages + refresh links
  .\bootstrap.ps1 link            # Create/refresh symlinks only (idempotent)
  .\bootstrap.ps1 unlink          # Remove only managed symlinks (safe)
  .\bootstrap.ps1 doctor [--fix]  # Diagnose + optionally remove broken symlinks
  .\bootstrap.ps1 -h | --help

All operations are non-interactive and safe to re-run on configured machines.
Requires Windows Developer Mode (or Admin) for symlink creation.
"@
}

# Parse arguments - designed to feel similar to the bash version
param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Arguments
)

if ($Arguments -contains "-h" -or $Arguments -contains "--help") {
    Show-Help
    exit 0
}

$Command = ""
$DoctorFix = $false

$positional = @()
foreach ($arg in $Arguments) {
    $lower = $arg.ToLower()
    if ($lower -eq "--fix" -or $lower -eq "-fix") {
        $DoctorFix = $true
    } elseif (-not $arg.StartsWith("-")) {
        $positional += $lower
    }
}

if ($positional.Count -gt 0) {
    $Command = $positional[0]
}

switch ($Command) {
    "" {
        # Default: full bootstrap (Scoop ensure + packages from manifest + links)
        Install-Scoop
        Write-Host ""
        Install-ScoopPackages
        Setup-Links
        Write-Host ""
        Write-Host "Done."
        Write-Host "Recommended: .\bootstrap.ps1 doctor"
    }
    "bootstrap" {
        Install-Scoop
        Write-Host ""
        Install-ScoopPackages
        Setup-Links
        Write-Host ""
        Write-Host "Done."
        Write-Host "Recommended: .\bootstrap.ps1 doctor"
    }
    "link" {
        Setup-Links
        Write-Host "Done."
        Write-Host "Recommended: .\bootstrap.ps1 doctor"
    }
    "unlink" {
        Unlink-Dotfiles
        Write-Host "Done."
    }
    "update" {
        Install-Scoop
        Write-Host ""
        # Update Scoop itself and all installed apps for latest versions post-pull
        scoop update
        scoop update * 2>$null | Out-Null
        Install-ScoopPackages
        Setup-Links
        Write-Host ""
        Write-Host "Update complete."
        Write-Host "Recommended: .\bootstrap.ps1 doctor"
    }
    "doctor" {
        Doctor -Fix:$DoctorFix
        Write-Host "Done."
    }
    default {
        Write-Host "Unknown command: $Command"
        Write-Host ""
        Show-Help
        exit 1
    }
}
