#Requires -Version 5.0
<#
.SYNOPSIS
    Windows bootstrap script for the dotfiles repository.
.DESCRIPTION
    PowerShell equivalent of the Unix 'bootstrap' script.
    The function words and command flow intentionally mirror the bash version,
    while keeping PowerShell-style function names.
#>

param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]]$Arguments
)

$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Definition
$MainMode = ""
$DoctorFix = $false
$LinkScanSkipDirectoryNames = @(
    "myenv",
    ".venv",
    "venv",
    "env",
    "node_modules",
    "__pycache__",
    ".mypy_cache",
    ".pytest_cache",
    ".ruff_cache",
    "target",
    "build",
    "dist",
    "straight",
    "var",
    "elpa",
    "eln",
    "auto-save-list",
    "backups",
    "transient",
    ".cache"
)

# =============================================================================
# CLI Argument Parsing & Initialization
# =============================================================================

function Show-Usage {
    Write-Host @"
bootstrap.ps1 - Windows dotfiles environment bootstrapper

Usage:
  .\bootstrap.ps1 [bootstrap]     # Full bootstrap / re-deploy (Scoop + MSYS2/pacman + links)
  .\bootstrap.ps1 update          # Update Scoop + MSYS2 packages + refresh links + clean broken links
  .\bootstrap.ps1 link            # Create or refresh links only (idempotent, safe refresh)
  .\bootstrap.ps1 unlink          # Remove only links managed by this repo (safe)
  .\bootstrap.ps1 doctor [--fix]  # Diagnose + optionally remove broken links
  .\bootstrap.ps1 -h | --help     # Show this help

All operations are non-interactive and safe to re-run on configured machines.
Conflict backups are created only for real items that would be overwritten.
Requires Windows Developer Mode (or Admin) for symlink creation.
"@
}

function Initialize-Arguments {
    if (-not $script:Arguments) {
        $script:Arguments = @()
    }

    if ($script:Arguments.Count -eq 1 -and $script:Arguments[0] -in @("-h", "--help")) {
        Show-Usage
        exit 0
    }

    if ($script:Arguments.Count -gt 0 -and $script:Arguments[0].StartsWith("-")) {
        Write-Host "Unknown option: $($script:Arguments[0])" -ForegroundColor Yellow
        Show-Usage
        exit 1
    }

    if ($script:Arguments.Count -gt 0) {
        $script:MainMode = $script:Arguments[0].ToLowerInvariant()
        $remaining = @()
        if ($script:Arguments.Count -gt 1) {
            $remaining = $script:Arguments[1..($script:Arguments.Count - 1)]
        }

        foreach ($arg in $remaining) {
            if ($arg -eq "--fix" -or $arg -eq "-Fix") {
                if ($script:MainMode -eq "doctor") {
                    $script:DoctorFix = $true
                } else {
                    Write-Host "Error: --fix is only valid with the 'doctor' subcommand." -ForegroundColor Yellow
                    Show-Usage
                    exit 1
                }
            } else {
                Write-Host "Unknown option or argument: $arg" -ForegroundColor Yellow
                Show-Usage
                exit 1
            }
        }
    }

    switch ($script:MainMode) {
        "" { }
        "link" { }
        "unlink" { }
        "update" { }
        "bootstrap" { }
        "doctor" { }
        default {
            Write-Host "Unknown subcommand: $script:MainMode" -ForegroundColor Yellow
            Show-Usage
            exit 1
        }
    }
}

function Main {
    if ([string]::IsNullOrEmpty($script:MainMode) -or $script:MainMode -eq "bootstrap") {
        Perform-FullBootstrap
        return
    }

    if ($script:MainMode -eq "link") {
        Setup-Links
    }

    if ($script:MainMode -eq "unlink") {
        Unlink-Dotfiles
    }

    if ($script:MainMode -eq "doctor") {
        Doctor -Fix:$script:DoctorFix
    }

    if ($script:MainMode -eq "update") {
        Update-Scoop
        Update-ScoopPackages
        Update-MSYS2Packages
        Update-RPackages
        Install-ZshPlugins
        Setup-Links
        Show-RestartNotice
    }
}

function Write-PrintLine {
    param(
        [string]$Left,
        [string]$Right,
        [string]$FillChar = "."
    )

    $cols = 80
    try {
        if ($Host.UI.RawUI.WindowSize.Width -gt 0) {
            $cols = $Host.UI.RawUI.WindowSize.Width
        }
    } catch {
        $cols = 80
    }

    $totalLength = $Left.Length + $Right.Length
    $lines = [Math]::Max(1, [Math]::Ceiling($totalLength / $cols))
    $fillLength = [Math]::Max(0, ($lines * $cols) - $totalLength)
    $filler = $FillChar * $fillLength

    Write-Host "$Left$filler$Right"
}

# =============================================================================
# Low-level Link Utilities
# =============================================================================

function Get-PathItem {
    param([string]$Path)
    return Get-Item -LiteralPath $Path -Force -ErrorAction SilentlyContinue
}

function Get-CanonicalPath {
    param(
        [string]$Path,
        [string]$BaseDir = ""
    )

    if ([string]::IsNullOrWhiteSpace($Path)) {
        return ""
    }

    $candidate = $Path
    if ($BaseDir -and -not [System.IO.Path]::IsPathRooted($candidate)) {
        $candidate = Join-Path $BaseDir $candidate
    }

    try {
        $resolved = Resolve-Path -LiteralPath $candidate -ErrorAction Stop
        return [System.IO.Path]::GetFullPath($resolved.ProviderPath).TrimEnd('\')
    } catch {
        try {
            return [System.IO.Path]::GetFullPath($candidate).TrimEnd('\')
        } catch {
            return $candidate.TrimEnd('\')
        }
    }
}

function Get-LinkTargetPath {
    param([string]$Path)

    $item = Get-PathItem $Path
    if (-not $item -or $item.LinkType -ne "SymbolicLink") {
        return ""
    }

    $target = "$($item.Target)"
    if (-not $target) {
        return ""
    }

    return Get-CanonicalPath $target (Split-Path -Parent $Path)
}

function Test-SamePath {
    param(
        [string]$Left,
        [string]$Right
    )

    $leftCanonical = Get-CanonicalPath $Left
    $rightCanonical = Get-CanonicalPath $Right
    return [string]::Equals($leftCanonical, $rightCanonical, [System.StringComparison]::OrdinalIgnoreCase)
}

function Test-LinkScanSkippedDirectory {
    param([object]$Item)

    if (-not $Item -or -not $Item.PSIsContainer) {
        return $false
    }

    return $script:LinkScanSkipDirectoryNames -contains $Item.Name
}

function Test-LinkScanDescendableDirectory {
    param([object]$Item)

    if (-not $Item -or -not $Item.PSIsContainer) {
        return $false
    }

    if ($Item.LinkType) {
        return $false
    }

    if (($Item.Attributes -band [System.IO.FileAttributes]::ReparsePoint) -ne 0) {
        return $false
    }

    return -not (Test-LinkScanSkippedDirectory $Item)
}

function Ensure-RealDirectory {
    param([string]$Path)

    $item = Get-PathItem $Path
    if ($item -and $item.LinkType -eq "SymbolicLink") {
        Write-Host "Replacing link with real directory: $Path"
        Remove-Item -LiteralPath $Path -Force
    }

    New-Item -ItemType Directory -Path $Path -Force | Out-Null
}

function Link-DirectoryContents {
    param(
        [string]$SourceDir,
        [string]$TargetDir
    )

    if (-not (Test-Path -LiteralPath $SourceDir -PathType Container)) {
        return
    }

    Ensure-RealDirectory $TargetDir

    Get-ChildItem -LiteralPath $SourceDir -Force | ForEach-Object {
        $name = $_.Name
        if ($name -in @(".", "..", ".DS_Store")) {
            return
        }

        $dest = Join-Path $TargetDir $name
        $sourceTarget = Get-LinkTargetPath $_.FullName
        if ($_.LinkType -eq "SymbolicLink" -and $sourceTarget -and (Test-SamePath $sourceTarget $TargetDir)) {
            Write-Host "Skipping circular link: $($_.FullName) -> $TargetDir"
            return
        }

        $destItem = Get-PathItem $dest
        if ($destItem -and (Test-SamePath $_.FullName $dest)) {
            Write-Host "Skipping existing correct link: $dest"
            return
        }

        if ($destItem -and $destItem.LinkType -eq "SymbolicLink") {
            $destTarget = Get-LinkTargetPath $dest
            if ($destTarget -and (Test-SamePath $destTarget $_.FullName)) {
                return
            }

            Write-Host "Replacing link: $dest"
            Remove-Item -LiteralPath $dest -Force
        } elseif ($destItem) {
            Backup-ConflictingFile $dest
        }

        try {
            New-Item -ItemType SymbolicLink -Path $dest -Target $_.FullName -Force -ErrorAction Stop | Out-Null
            Write-Host "Linked: $dest -> $($_.FullName)"
        } catch {
            Write-Host "Failed to create symlink: $dest -> $($_.FullName)" -ForegroundColor Yellow
            Write-Host "Windows may require Developer Mode or Admin rights for symlinks." -ForegroundColor Yellow
        }
    }
}

function Link-Tree {
    param(
        [string]$SourceDir,
        [string]$TargetDir
    )

    if (-not (Test-Path -LiteralPath $SourceDir -PathType Container)) {
        return
    }

    Ensure-RealDirectory $TargetDir

    Get-ChildItem -LiteralPath $SourceDir -Force | ForEach-Object {
        $name = $_.Name
        if ($name -in @(".", "..", ".DS_Store")) {
            return
        }

        $dest = Join-Path $TargetDir $name
        $sourceTarget = Get-LinkTargetPath $_.FullName
        if ($_.LinkType -eq "SymbolicLink" -and $sourceTarget -and (Test-SamePath $sourceTarget $TargetDir)) {
            Write-Host "Skipping circular link: $($_.FullName) -> $TargetDir"
            return
        }

        $destItem = Get-PathItem $dest
        if ($destItem -and (Test-SamePath $_.FullName $dest)) {
            Write-Host "Skipping existing correct link: $dest"
            return
        }

        if ($_.PSIsContainer -and $_.LinkType -ne "SymbolicLink") {
            Link-Tree $_.FullName $dest
            return
        }

        if ($destItem -and $destItem.LinkType -eq "SymbolicLink") {
            $destTarget = Get-LinkTargetPath $dest
            if ($destTarget -and (Test-SamePath $destTarget $_.FullName)) {
                return
            }

            Write-Host "Replacing link: $dest"
            Remove-Item -LiteralPath $dest -Force
        } elseif ($destItem) {
            Backup-ConflictingFile $dest
        }

        try {
            New-Item -ItemType SymbolicLink -Path $dest -Target $_.FullName -Force -ErrorAction Stop | Out-Null
            Write-Host "Linked: $dest -> $($_.FullName)"
        } catch {
            Write-Host "Failed to create symlink: $dest -> $($_.FullName)" -ForegroundColor Yellow
            Write-Host "Windows may require Developer Mode or Admin rights for symlinks." -ForegroundColor Yellow
        }
    }
}

function Is-BrokenLink {
    param([string]$Path)

    $item = Get-PathItem $Path
    if (-not $item -or $item.LinkType -ne "SymbolicLink") {
        return $false
    }

    $target = "$($item.Target)"
    if (-not $target) {
        return $true
    }

    $candidate = $target
    if (-not [System.IO.Path]::IsPathRooted($candidate)) {
        $candidate = Join-Path (Split-Path -Parent $Path) $candidate
    }

    return -not (Test-Path -LiteralPath $candidate)
}

function Is-ManagedLink {
    param([string]$Path)

    $item = Get-PathItem $Path
    if (-not $item -or $item.LinkType -ne "SymbolicLink") {
        return $false
    }

    $target = "$($item.Target)"
    if (-not $target) {
        return $false
    }

    $resolved = Get-CanonicalPath $target (Split-Path -Parent $Path)
    $repo = Get-CanonicalPath $script:ScriptDir

    return $resolved.StartsWith($repo, [System.StringComparison]::OrdinalIgnoreCase)
}

# =============================================================================
# Link Scanning & Safety Helpers
# =============================================================================

function Get-SymlinkChildren {
    param(
        [string]$BaseDir,
        [switch]$Recurse
    )

    if (-not (Test-Path -LiteralPath $BaseDir -PathType Container)) {
        return @()
    }

    $params = @{
        LiteralPath = $BaseDir
        Force = $true
        ErrorAction = "SilentlyContinue"
    }
    if (-not $Recurse) {
        return @(Get-ChildItem @params | Where-Object { $_.LinkType -eq "SymbolicLink" })
    }

    function Get-SymlinkChildrenRecursively {
        param([string]$CurrentDir)

        foreach ($child in (Get-ChildItem -LiteralPath $CurrentDir -Force -ErrorAction SilentlyContinue)) {
            if ($child.LinkType -eq "SymbolicLink") {
                $child
                continue
            }

            if (Test-LinkScanDescendableDirectory $child) {
                Get-SymlinkChildrenRecursively $child.FullName
            }
        }
    }

    return @(Get-SymlinkChildrenRecursively $BaseDir)
}

function Remove-BrokenLinksRecursivelyUnder {
    param([string]$BaseDir)

    foreach ($link in (Get-SymlinkChildren $BaseDir -Recurse)) {
        if (Is-BrokenLink $link.FullName) {
            Write-Host "Removing broken link: $($link.FullName)"
            Remove-Item -LiteralPath $link.FullName -Force
            continue
        }

        $target = Get-LinkTargetPath $link.FullName
        $repo = Get-CanonicalPath $script:ScriptDir
        if ($target -and $target.StartsWith($repo, [System.StringComparison]::OrdinalIgnoreCase) -and -not (Test-Path -LiteralPath $target)) {
            Write-Host "Removing orphan link (source moved/removed): $($link.FullName) -> $target"
            Remove-Item -LiteralPath $link.FullName -Force
        }
    }
}

function Remove-ManagedLinksRecursivelyUnder {
    param([string]$BaseDir)

    $removed = 0
    foreach ($link in (Get-SymlinkChildren $BaseDir -Recurse)) {
        if (Is-ManagedLink $link.FullName) {
            $target = Get-LinkTargetPath $link.FullName
            Write-Host "Removing managed link: $($link.FullName) -> $target"
            Remove-Item -LiteralPath $link.FullName -Force
            $removed++
        }
    }

    if ($removed -eq 0) {
        Write-Host "  (no managed links found)"
    }
}

function Report-BrokenLinksRecursivelyUnder {
    param([string]$BaseDir)

    foreach ($link in (Get-SymlinkChildren $BaseDir -Recurse)) {
        if (Is-BrokenLink $link.FullName) {
            Write-Host "Broken: $($link.FullName)"
            continue
        }

        $target = Get-LinkTargetPath $link.FullName
        $repo = Get-CanonicalPath $script:ScriptDir
        if ($target -and $target.StartsWith($repo, [System.StringComparison]::OrdinalIgnoreCase) -and -not (Test-Path -LiteralPath $target)) {
            Write-Host "Orphan (managed by dotfiles but source gone): $($link.FullName) -> $target"
        }
    }
}

function Remove-BrokenLinksUnderTopLevel {
    param([string]$BaseDir)

    foreach ($link in (Get-SymlinkChildren $BaseDir)) {
        if (Is-BrokenLink $link.FullName) {
            Write-Host "Removing broken link: $($link.FullName)"
            Remove-Item -LiteralPath $link.FullName -Force
        }
    }
}

function Remove-ManagedLinksUnderTopLevel {
    param([string]$BaseDir)

    foreach ($link in (Get-SymlinkChildren $BaseDir)) {
        if (Is-ManagedLink $link.FullName) {
            $target = Get-LinkTargetPath $link.FullName
            Write-Host "Removing managed link: $($link.FullName) -> $target"
            Remove-Item -LiteralPath $link.FullName -Force
        }
    }
}

function Report-BrokenLinksUnderTopLevel {
    param([string]$BaseDir)

    foreach ($link in (Get-SymlinkChildren $BaseDir)) {
        if (Is-BrokenLink $link.FullName) {
            Write-Host "Broken: $($link.FullName)"
        }
    }
}

function Backup-ConflictingFile {
    param([string]$Path)

    $timestamp = Get-Date -Format "yyyyMMdd-HHmmss"
    $localAppData = $env:LOCALAPPDATA
    if (-not $localAppData) {
        $localAppData = Join-Path $HOME "AppData\Local"
    }

    $backupRoot = Join-Path $localAppData "dotfiles\backups\$timestamp"
    $homeCanonical = Get-CanonicalPath $HOME
    $pathCanonical = Get-CanonicalPath $Path
    $relative = $pathCanonical
    if ($pathCanonical.StartsWith($homeCanonical, [System.StringComparison]::OrdinalIgnoreCase)) {
        $relative = $pathCanonical.Substring($homeCanonical.Length).TrimStart('\')
    }

    $backupPath = Join-Path $backupRoot $relative
    New-Item -ItemType Directory -Path (Split-Path -Parent $backupPath) -Force | Out-Null
    Move-Item -LiteralPath $Path -Destination $backupPath -Force

    Write-Host "Conflict: $Path was a real item."
    Write-Host "    Backed up to: $backupPath"
    Write-Host "    Proceeding to create link."
}

# =============================================================================
# High-level Link Operations
# =============================================================================

function Setup-Links {
    $leftMessage = "Linking dotfiles"
    Write-PrintLine $leftMessage "Started."

    $dotfilesRoot = $script:ScriptDir

    Remove-BrokenLinksUnderTopLevel $HOME

    foreach ($dir in @(
        (Join-Path $HOME ".config"),
        (Join-Path $HOME ".local"),
        (Join-Path $HOME ".emacs.d")
    )) {
        Remove-BrokenLinksRecursivelyUnder $dir
    }

    Link-Tree (Join-Path $dotfilesRoot "home") $HOME
    Link-Tree (Join-Path $dotfilesRoot "config") (Join-Path $HOME ".config")
    Link-Tree (Join-Path $dotfilesRoot "emacs.d") (Join-Path $HOME ".emacs.d")

    Ensure-RealDirectory (Join-Path $HOME ".local")
    Ensure-RealDirectory (Join-Path $HOME ".local\bin")
    Link-DirectoryContents (Join-Path $dotfilesRoot "local\bin") (Join-Path $HOME ".local\bin")

    Write-PrintLine $leftMessage "Finished."
}

function Unlink-Dotfiles {
    $leftMessage = "Unlinking dotfiles"
    Write-PrintLine $leftMessage "Started."

    Write-Host "This will remove links that point back into this dotfiles repository."
    Write-Host "Only links created by 'bootstrap link' (or equivalent) will be touched."
    Write-Host "Real files and links created by other tools will be left alone."
    Write-Host ""

    Write-Host "=== $HOME (top level only) ==="
    Remove-ManagedLinksUnderTopLevel $HOME

    foreach ($dir in @(
        (Join-Path $HOME ".config"),
        (Join-Path $HOME ".local"),
        (Join-Path $HOME ".emacs.d")
    )) {
        Write-Host ""
        Write-Host "=== $dir ==="
        Remove-ManagedLinksRecursivelyUnder $dir
    }

    Write-Host ""
    Write-PrintLine $leftMessage "Finished."
}

function Doctor {
    param([switch]$Fix)

    $leftMessage = "Running doctor"
    Write-PrintLine $leftMessage "Started."

    Write-Host "Scanning for broken links under managed locations (safe, limited scope)..."
    Write-Host ""
    Write-Host "Note: Only direct children of $HOME + full contents of ~/.config, ~/.local, and ~/.emacs.d are considered."
    Write-Host "      Deep recursion under raw HOME is intentionally avoided."
    Write-Host "      Generated dependency directories and link target trees are skipped."
    Write-Host ""

    if ($Fix) {
        Write-Host "==> Fix mode enabled."
        Write-Host ""
    }

    Write-Host "=== $HOME (top level only) ==="
    if ($Fix) {
        Remove-BrokenLinksUnderTopLevel $HOME
    } else {
        Report-BrokenLinksUnderTopLevel $HOME
    }

    foreach ($dir in @(
        (Join-Path $HOME ".config"),
        (Join-Path $HOME ".local"),
        (Join-Path $HOME ".emacs.d")
    )) {
        Write-Host ""
        Write-Host "=== $dir ==="
        if ($Fix) {
            Remove-BrokenLinksRecursivelyUnder $dir
        } else {
            Report-BrokenLinksRecursivelyUnder $dir
        }
    }

    Write-Host ""
    if ($Fix) {
        Write-PrintLine $leftMessage "Finished (fix mode)."
    } else {
        Write-PrintLine $leftMessage "Finished (report only)."
        Write-Host ""
        Write-Host "Tip: Run '.\bootstrap.ps1 doctor --fix' to remove broken links."
    }
}

# =============================================================================
# Package Management Utilities
# =============================================================================

function Install-Scoop {
    if (Get-Command scoop -ErrorAction SilentlyContinue) {
        return
    }

    $leftMessage = "Installing Scoop"
    Write-PrintLine $leftMessage "Started."

    Set-ExecutionPolicy RemoteSigned -Scope CurrentUser -Force
    try {
        Invoke-RestMethod -UseBasicParsing get.scoop.sh | Invoke-Expression
    } catch {
        Write-Host "Failed to install Scoop. Please install it manually from https://scoop.sh" -ForegroundColor Yellow
        exit 1
    }

    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path", "User")

    Write-PrintLine $leftMessage "Finished."
}

function Update-Scoop {
    if (-not (Get-Command scoop -ErrorAction SilentlyContinue)) {
        return
    }

    $leftMessage = "Updating Scoop"
    Write-PrintLine $leftMessage "Started."

    scoop update

    Write-PrintLine $leftMessage "Finished."
}

function Install-ScoopPackages {
    if (-not (Get-Command scoop -ErrorAction SilentlyContinue)) {
        return
    }

    $leftMessage = "Installing Scoop packages (via scoopfile)"
    Write-PrintLine $leftMessage "Started."

    $requiredBuckets = @("extras")
    foreach ($bucket in $requiredBuckets) {
        if (-not (scoop bucket list | Select-String "^$bucket$")) {
            Write-Host "Adding required Scoop bucket: $bucket"
            scoop bucket add $bucket | Out-Null
        }
    }

    $scoopfile = Join-Path (Join-Path (Join-Path $script:ScriptDir "pkg") "scoop") "scoopfile.json"
    if (Test-Path -LiteralPath $scoopfile) {
        scoop import $scoopfile
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Warning: Some packages from the scoopfile may have failed." -ForegroundColor Yellow
        }
    } else {
        Write-Host "No scoopfile found. Using fallback list."
        $packages = @("git", "ripgrep", "fzf", "emacs", "r", "python", "sbcl")
        foreach ($pkg in $packages) {
            if (-not (scoop list | Select-String "^$pkg\s")) {
                scoop install $pkg
            }
        }
    }

    Write-PrintLine $leftMessage "Finished."
}

function Update-ScoopPackages {
    if (-not (Get-Command scoop -ErrorAction SilentlyContinue)) {
        return
    }

    $leftMessage = "Updating Scoop packages (via scoopfile)"
    Write-PrintLine $leftMessage "Started."

    scoop update
    scoop update *
    Install-ScoopPackages

    Write-PrintLine $leftMessage "Finished."
}

function Get-MSYS2BashPath {
    # Returns the path to the Scoop-installed MSYS2 bash.exe, or $null if MSYS2
    # is not available via Scoop.
    if (-not (Get-Command scoop -ErrorAction SilentlyContinue)) {
        return $null
    }

    $prefix = $null
    try {
        $prefix = (scoop prefix msys2 2>$null | Select-Object -First 1)
        if ($prefix) { $prefix = $prefix.Trim() }
    } catch {
        $prefix = $null
    }
    if (-not $prefix -or -not (Test-Path -LiteralPath $prefix)) {
        return $null
    }

    $bash = Join-Path $prefix "usr\bin\bash.exe"
    if (-not (Test-Path -LiteralPath $bash)) {
        return $null
    }
    return $bash
}

function Install-MSYS2Packages {
    # Installs MSYS2/ucrt64 shell tools from pkg/pacman/msys2-packages.txt by
    # invoking the Scoop-installed MSYS2 bash with MSYSTEM=UCRT64. Mirrors
    # install_pacman_packages() in the Unix 'bootstrap'. Idempotent (--needed);
    # safe no-op if MSYS2 (via Scoop) is unavailable.
    $bash = Get-MSYS2BashPath
    if (-not $bash) {
        Write-Host "MSYS2 (via Scoop) not found; skipping pacman packages." -ForegroundColor Yellow
        return
    }

    $listFile = Join-Path (Join-Path (Join-Path $script:ScriptDir "pkg") "pacman") "msys2-packages.txt"
    if (-not (Test-Path -LiteralPath $listFile)) {
        return
    }

    $leftMessage = "Installing MSYS2 packages (via pacman/pacboy)"
    Write-PrintLine $leftMessage "Started."

    # pacboy (from pactoys) resolves `name:p` to the active $MSYSTEM prefix.
    # The script is written to a temp file (UTF-8, no BOM, LF) and run as
    # `bash -l <posix-path>` rather than `bash -lc "<script>"`: passing a
    # multi-line, nested-quote script across the Windows -> bash.exe argument
    # boundary corrupts it (the command line is re-parsed and a UTF-8 BOM is
    # prepended). A file + a single path argument sidesteps both problems.
    # The package list path is passed via the environment and converted with
    # cygpath inside MSYS2.
    $bashScript = (@'
set -e
command -v pacboy >/dev/null 2>&1 || pacman -S --needed --noconfirm pactoys
list="$(cygpath -u "$DOTFILES_PKGLIST")"
mapfile -t pkgs < <(sed -e 's/[[:space:]]*#.*$//' -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' "$list" | grep -v '^[[:space:]]*$')
if [ "${#pkgs[@]}" -gt 0 ]; then
  pacboy -S --needed --noconfirm "${pkgs[@]}"
fi
'@) -replace "`r", ""

    $tmp = [System.IO.Path]::GetTempFileName()
    $savedMsystem = $env:MSYSTEM
    $savedPkglist = $env:DOTFILES_PKGLIST
    try {
        [System.IO.File]::WriteAllText($tmp, $bashScript, (New-Object System.Text.UTF8Encoding $false))
        # Convert the temp path with cygpath; pass it as $0 (single-level quotes,
        # no nesting) so the argument survives the Windows -> bash.exe boundary.
        $posix = (& $bash -lc 'cygpath -u "$0"' $tmp | Select-Object -First 1).Trim()

        $env:MSYSTEM = "UCRT64"
        $env:DOTFILES_PKGLIST = $listFile
        & $bash -l $posix
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Warning: some MSYS2 packages may have failed (exit $LASTEXITCODE)." -ForegroundColor Yellow
        }
    } finally {
        $env:MSYSTEM = $savedMsystem
        $env:DOTFILES_PKGLIST = $savedPkglist
        [System.IO.File]::Delete($tmp)
    }

    Write-PrintLine $leftMessage "Finished."
}

function Update-MSYS2Packages {
    # Full MSYS2 system upgrade via pacman. Mirrors update_pacman_packages().
    $bash = Get-MSYS2BashPath
    if (-not $bash) {
        return
    }

    $leftMessage = "Updating MSYS2 packages (via pacman)"
    Write-PrintLine $leftMessage "Started."

    $savedMsystem = $env:MSYSTEM
    try {
        $env:MSYSTEM = "UCRT64"
        & $bash -lc "pacman -Syu --noconfirm"
    } finally {
        $env:MSYSTEM = $savedMsystem
    }

    Write-PrintLine $leftMessage "Finished."
}

function Install-RPackages {
    if (-not (Get-Command Rscript -ErrorAction SilentlyContinue)) {
        return
    }

    $leftMessage = "Installing or updating R packages"
    Write-PrintLine $leftMessage "Started."

    Rscript -e "packages <- c('tidyverse'); install.packages(packages[!packages %in% installed.packages()[, 'Package']], repos='https://cran.rstudio.com')" 2>$null

    Write-PrintLine $leftMessage "Finished."
}

function Update-RPackages {
    if (-not (Get-Command Rscript -ErrorAction SilentlyContinue)) {
        return
    }

    $leftMessage = "Updating installed R packages"
    Write-PrintLine $leftMessage "Started."

    Rscript -e "update.packages(ask = FALSE)" 2>$null

    Write-PrintLine $leftMessage "Finished."
}

function Install-ZshPlugins {
    # Clones the zsh plugins listed in pkg/zsh-plugins/plugins.txt by invoking
    # the Scoop-installed MSYS2 bash, so the repos land in exactly the path the
    # MSYS2 zsh reads ($XDG_DATA_HOME/zsh/plugins). Mirrors install_zsh_plugins()
    # in the Unix 'bootstrap' (single clone implementation, in bash). Idempotent;
    # no-op if MSYS2 (via Scoop) or git is unavailable.
    $bash = Get-MSYS2BashPath
    if (-not $bash) {
        Write-Host "MSYS2 (via Scoop) not found; skipping zsh plugins." -ForegroundColor Yellow
        return
    }

    $listFile = Join-Path (Join-Path (Join-Path $script:ScriptDir "pkg") "zsh-plugins") "plugins.txt"
    if (-not (Test-Path -LiteralPath $listFile)) {
        return
    }

    $leftMessage = "Installing zsh plugins"
    Write-PrintLine $leftMessage "Started."

    # Same temp-file + env-var + cygpath approach as Install-MSYS2Packages: a
    # multi-line bash script across the Windows -> bash.exe boundary is corrupted
    # if passed via -lc, so write it to a file and run `bash -l <posix-path>`.
    # The list path is passed via $DOTFILES_PKGLIST and converted with cygpath.
    $bashScript = (@'
set -e
command -v git >/dev/null 2>&1 || exit 0
list="$(cygpath -u "$DOTFILES_PKGLIST")"
plugin_dir="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/plugins"
mkdir -p "$plugin_dir"
while IFS= read -r url || [ -n "$url" ]; do
  url=$(printf '%s' "$url" | sed -e 's/[[:space:]]*#.*$//' -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
  [ -z "$url" ] && continue
  name="$(basename "$url")"
  dest="$plugin_dir/$name"
  if [ -d "$dest/.git" ]; then
    git -C "$dest" pull --ff-only --quiet || echo "Could not update $name"
  else
    git clone --depth 1 "$url" "$dest"
  fi
done < "$list"
'@) -replace "`r", ""

    $tmp = [System.IO.Path]::GetTempFileName()
    $savedPkglist = $env:DOTFILES_PKGLIST
    try {
        [System.IO.File]::WriteAllText($tmp, $bashScript, (New-Object System.Text.UTF8Encoding $false))
        $posix = (& $bash -lc 'cygpath -u "$0"' $tmp | Select-Object -First 1).Trim()

        $env:DOTFILES_PKGLIST = $listFile
        & $bash -l $posix
        if ($LASTEXITCODE -ne 0) {
            Write-Host "Warning: some zsh plugins may have failed (exit $LASTEXITCODE)." -ForegroundColor Yellow
        }
    } finally {
        $env:DOTFILES_PKGLIST = $savedPkglist
        [System.IO.File]::Delete($tmp)
    }

    Write-PrintLine $leftMessage "Finished."
}

function Perform-FullBootstrap {
    $leftMessage = "Full bootstrap"
    Write-PrintLine $leftMessage "Started."

    Install-Scoop
    Install-ScoopPackages
    Install-MSYS2Packages
    Install-RPackages
    Install-ZshPlugins
    Setup-Links

    Write-PrintLine $leftMessage "Finished."
}

# =============================================================================
# Support & Reporting Functions
# =============================================================================

function Show-RestartNotice {
    $mode = $script:MainMode
    if ([string]::IsNullOrEmpty($mode)) {
        $mode = "bootstrap"
    }

    Write-Host ""
    Write-Host "Bootstrap step '$mode' complete. To apply PATH or shell changes, restart PowerShell."
    Write-Host ""
}

Initialize-Arguments
Main
