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
  .\bootstrap.ps1 [bootstrap]     # Full bootstrap / re-deploy (Scoop + links)
  .\bootstrap.ps1 update          # Update Scoop packages + refresh links + clean broken links
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
        Setup-DevPackages
    }

    if ($script:MainMode -eq "unlink") {
        Unlink-Dotfiles
    }

    if ($script:MainMode -eq "doctor") {
        Doctor -Fix:$script:DoctorFix
    }

    if ($script:MainMode -eq "update") {
        Update-Scoop
        Set-UserEnvironment
        Update-ScoopPackages
        Install-Fonts
        Setup-Links
        Setup-DevPackages
        Install-Cmigemo
        Setup-StartupShortcuts
        Setup-OrgProtocol
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

    if ($script:LinkScanSkipDirectoryNames -contains $Item.Name) {
        return $true
    }

    # `env` is skipped only where it is a virtualenv.  The name is a word, and
    # one of the directories this repository deploys is called it --
    # config/shell/env, which holds the modules the shells source -- so
    # skipping by name alone hid every link under it from the scan, in report
    # mode as well as in fix mode.  A virtualenv carries a pyvenv.cfg (PEP 405).
    if ($Item.Name -eq "env") {
        return (Test-Path -LiteralPath (Join-Path $Item.FullName "pyvenv.cfg"))
    }

    return $false
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

function Link-GitHooks {
    # The checks every commit here goes through -- pre-commit (leakseek,
    # gitleaks, the Japanese and package scope checks) and commit-msg -- linked
    # into the repository's .git\hooks from ~/.local/bin.  Linked rather than
    # copied, so a change to a check reaches every repository at once.  A hook
    # already there that is something else is replaced.  A repository made
    # with `git init' or cloned has none of these, and its commits go
    # unchecked until it does.  Git for Windows runs them with its own bash.
    param([string]$Repo)

    $gitDir = Join-Path $Repo ".git"
    if (-not (Test-Path -LiteralPath $gitDir -PathType Container)) {
        return
    }
    $hooks = Join-Path $gitDir "hooks"
    New-Item -ItemType Directory -Path $hooks -Force | Out-Null

    foreach ($hook in @("pre-commit", "commit-msg")) {
        $src = Join-Path $HOME ".local\bin\$hook"
        $dest = Join-Path $hooks $hook
        $destItem = Get-PathItem $dest
        if ($destItem -and $destItem.LinkType -eq "SymbolicLink") {
            $destTarget = Get-LinkTargetPath $dest
            if ($destTarget -and (Test-SamePath $destTarget $src)) {
                continue
            }
        }
        if ($destItem) {
            Remove-Item -LiteralPath $dest -Force
        }
        try {
            New-Item -ItemType SymbolicLink -Path $dest -Target $src -Force -ErrorAction Stop | Out-Null
            Write-Host "Hook linked: $dest -> $src"
        } catch {
            Write-Host "Failed to link hook ${dest}: $($_.Exception.Message)" -ForegroundColor Yellow
        }
    }
}

function Report-MissingLeakseekConf {
    # The hooks abort every commit until this machine has a keyword list: a
    # check with nothing to check against would pass everything and say
    # nothing.  The list, and the allow file beside it, never leave the
    # machine, so bootstrap cannot bring them; it says what is missing.
    $configDir = $env:XDG_CONFIG_HOME
    if (-not $configDir) {
        $configDir = Join-Path $HOME ".config"
    }
    $conf = Join-Path $configDir ".leakseek.conf"
    if (-not (Test-Path -LiteralPath $conf -PathType Leaf)) {
        Write-Host "No leakseek keyword list at ${conf}: commits in the repositories" -ForegroundColor Yellow
        Write-Host "    with hooks are aborted until there is one.  'leakseek --template'" -ForegroundColor Yellow
        Write-Host "    (from Git Bash) writes an empty one to fill in." -ForegroundColor Yellow
    }
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

    # This repository's own commits go through the hooks just linked into
    # ~/.local/bin, on every machine it is cloned to; Setup-DevPackages does
    # the same for the packages.
    Link-GitHooks $dotfilesRoot
    Report-MissingLeakseekConf

    # Skills are the one part of ~/.claude worth tracking: plain files that never
    # get rewritten, and rules that have to be in front of every project -- the
    # packages in ~/Developer included, which this repo does not otherwise reach.
    Ensure-RealDirectory (Join-Path $HOME ".claude")
    Ensure-RealDirectory (Join-Path $HOME ".claude\skills")
    Link-DirectoryContents (Join-Path $dotfilesRoot "claude\skills") (Join-Path $HOME ".claude\skills")

    # The rest of ~/.claude is intentionally not managed: settings.json is
    # Claude-owned and rewritten at runtime (model/theme/effortLevel), so
    # symlinking it into the repo produces perpetual diff noise. Claude Code
    # creates ~/.claude itself.

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
    Write-Host "=== Startup shortcuts ==="
    Remove-StartupShortcuts

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
        $packages = @("git", "ripgrep", "emacs")
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

function Install-Fonts {
    # The two fonts this configuration draws with, neither of which Windows
    # builds or installs by itself here:
    #
    #   PlemolJP NF          for Emacs.  Built on the Mac by
    #                        local/bin/build-plemoljp-nf and carried over by
    #                        hand: it is a modified PlemolJP, which OFL lets
    #                        be used but not given out under that name, so it
    #                        is in no repository to download from.
    #   PlemolJP Console NF  for mintty.  Upstream's own, unmodified, fetched
    #                        here from its release at the version the Mac
    #                        builds from, and checked against the digest
    #                        GitHub records for it.
    #
    # Neither is installed from here: a font is installed per user from the
    # right-click menu, and that is the one way Windows keeps the registry
    # and the session in step without a script doing it by hand.  So this
    # says what is missing, and for Console NF puts the four files where the
    # right-click is.  Idempotent: silent once both are installed.
    $userFonts = Join-Path $env:LOCALAPPDATA "Microsoft\Windows\Fonts"
    $sysFonts  = Join-Path $env:WINDIR "Fonts"
    function Test-FontInstalled([string]$file) {
        (Test-Path -LiteralPath (Join-Path $userFonts $file)) -or
        (Test-Path -LiteralPath (Join-Path $sysFonts $file))
    }

    $haveNF      = Test-FontInstalled "PlemolJPNF-Regular.ttf"
    $haveConsole = Test-FontInstalled "PlemolJPConsoleNF-Regular.ttf"
    if ($haveNF -and $haveConsole) {
        return
    }

    $leftMessage = "Checking fonts"
    Write-PrintLine $leftMessage "Started."

    if (-not $haveNF) {
        Write-Host "PlemolJP NF is not installed. Build it on the Mac (build-plemoljp-nf)," -ForegroundColor Yellow
        Write-Host "  bring the four PlemolJPNF-*.ttf here, and right-click each > Install." -ForegroundColor Yellow
    }

    if (-not $haveConsole) {
        $version = "3.0.0"
        $sha256  = "887E2D0DB714D41AC26AB924BDD3A172B922F57B2B480AF01E0147180EAC3D80"
        $url     = "https://github.com/yuru7/PlemolJP/releases/download/v$version/PlemolJP_NF_v$version.zip"
        $dest    = Join-Path $HOME "Downloads\PlemolJP Console NF"
        $zip     = Join-Path $env:TEMP "PlemolJP_NF_v$version.zip"
        try {
            $oldProgress = $ProgressPreference
            $ProgressPreference = "SilentlyContinue"
            Invoke-WebRequest -Uri $url -OutFile $zip -UseBasicParsing
            $ProgressPreference = $oldProgress

            $actual = (Get-FileHash -LiteralPath $zip -Algorithm SHA256).Hash
            if ($actual -ne $sha256) {
                Write-Host "PlemolJP Console NF download checksum mismatch; skipping." -ForegroundColor Yellow
                Write-Host "    expected $sha256" -ForegroundColor Yellow
                Write-Host "    actual   $actual" -ForegroundColor Yellow
            } else {
                # Only the four faces mintty asks for: the other weights are
                # families of their own to GDI ("PlemolJP Console NF Thin").
                Add-Type -AssemblyName System.IO.Compression.FileSystem
                New-Item -ItemType Directory -Path $dest -Force | Out-Null
                $archive = [System.IO.Compression.ZipFile]::OpenRead($zip)
                try {
                    foreach ($style in @("Regular", "Bold", "Italic", "BoldItalic")) {
                        $name  = "PlemolJPConsoleNF-$style.ttf"
                        $entry = $archive.Entries | Where-Object { $_.FullName -like "*/PlemolJPConsole_NF/$name" } | Select-Object -First 1
                        if ($entry) {
                            [System.IO.Compression.ZipFileExtensions]::ExtractToFile($entry, (Join-Path $dest $name), $true)
                        }
                    }
                } finally {
                    $archive.Dispose()
                }
                Write-Host "PlemolJP Console NF is not installed. Its four faces are in:" -ForegroundColor Yellow
                Write-Host "  $dest -- right-click each > Install." -ForegroundColor Yellow
            }
        } catch {
            Write-Host "Could not fetch PlemolJP Console NF: $_" -ForegroundColor Yellow
        } finally {
            Remove-Item -LiteralPath $zip -Force -ErrorAction SilentlyContinue
        }
    }

    Write-PrintLine $leftMessage "Finished."
}

function Set-UserEnvironment {
    # Persist the user-scope environment variables Emacs and cmigemo rely on:
    #   HOME  -> so Emacs resolves ~ to the Windows user profile
    #   PATH  += <HOME>\.local\bin -> so cmigemo (and other ~/.local/bin shims)
    #           are found by executable-find
    # Idempotent: writes only when a value is missing or wrong. .NET's
    # SetEnvironmentVariable at User scope broadcasts WM_SETTINGCHANGE, so new
    # processes pick up the change without a logout.
    $leftMessage = "Configuring user environment variables"
    Write-PrintLine $leftMessage "Started."

    $homeValue = $HOME
    $currentHome = [System.Environment]::GetEnvironmentVariable("HOME", "User")
    if ($currentHome -ne $homeValue) {
        Write-Host "Setting user HOME = $homeValue"
        [System.Environment]::SetEnvironmentVariable("HOME", $homeValue, "User")
    } else {
        Write-Host "User HOME already set: $homeValue"
    }
    $env:HOME = $homeValue

    $localBin = Join-Path $HOME ".local\bin"
    $userPath = [System.Environment]::GetEnvironmentVariable("Path", "User")
    if (-not $userPath) { $userPath = "" }
    $entries = @($userPath -split ";" | Where-Object { $_ -ne "" })
    $already = @($entries | Where-Object { $_.TrimEnd('\') -ieq $localBin.TrimEnd('\') })
    if ($already.Count -eq 0) {
        Write-Host "Adding to user PATH: $localBin"
        $trimmed = $userPath.TrimEnd(';')
        $newPath = if ($trimmed) { "$trimmed;$localBin" } else { $localBin }
        [System.Environment]::SetEnvironmentVariable("Path", $newPath, "User")
    } else {
        Write-Host "User PATH already contains: $localBin"
    }

    # Reflect in the current session so later steps see it.
    $sessionEntries = @($env:Path -split ";" | Where-Object { $_.TrimEnd('\') -ieq $localBin.TrimEnd('\') })
    if ($sessionEntries.Count -eq 0) {
        $env:Path = $env:Path.TrimEnd(';') + ";" + $localBin
    }

    Write-PrintLine $leftMessage "Finished."
}

function Install-Cmigemo {
    # cmigemo is not available via Scoop, so fetch the KaoriYa Windows build and
    # lay it out exactly where emacs.d/modules/my-editor-search.el expects it:
    #   ~/.local/share/cmigemo/                        (migemo-directory)
    #   ~/.local/share/cmigemo/cmigemo.exe + migemo.dll
    #   ~/.local/share/cmigemo/dict/cp932/migemo-dict  (migemo-dictionary)
    # and expose the CLI on PATH via a symlink:
    #   ~/.local/bin/cmigemo.exe -> ~/.local/share/cmigemo/cmigemo.exe
    # cmigemo.exe loads migemo.dll from its own (real) directory, so a single
    # symlink to the exe is enough: Windows resolves the launched symlink to its
    # target, making the target directory the application directory used for the
    # default DLL search. Idempotent: no-op once both the exe and the link exist.
    $shareDir = Join-Path $HOME ".local\share\cmigemo"
    $exe      = Join-Path $shareDir "cmigemo.exe"
    $binLink  = Join-Path $HOME ".local\bin\cmigemo.exe"

    if ((Test-Path -LiteralPath $exe) -and (Test-Path -LiteralPath $binLink)) {
        return
    }

    $leftMessage = "Installing cmigemo (KaoriYa win64 build)"
    Write-PrintLine $leftMessage "Started."

    # KaoriYa's HTTPS certificate is expired; the plain-HTTP direct URL serves
    # the same file with no redirect. A pinned SHA-256 guards against a corrupted
    # or tampered archive over the unauthenticated transport.
    $url    = "http://files.kaoriya.net/cmigemo/cmigemo-default-win64-20110227.zip"
    $sha256 = "80adcf55848b46f8eb006ff4f73c5b840e7e322529d5d4e534be235ff0bb4ad0"

    $work = Join-Path $env:TEMP "cmigemo-install"
    if (Test-Path -LiteralPath $work) {
        Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
    }
    New-Item -ItemType Directory -Path $work -Force | Out-Null
    $zip = Join-Path $work "cmigemo.zip"

    try {
        $oldProgress = $ProgressPreference
        $ProgressPreference = "SilentlyContinue"
        Invoke-WebRequest -Uri $url -OutFile $zip -UseBasicParsing
        $ProgressPreference = $oldProgress

        $actual = (Get-FileHash -LiteralPath $zip -Algorithm SHA256).Hash
        if ($actual -ne $sha256) {
            Write-Host "cmigemo download checksum mismatch; skipping." -ForegroundColor Yellow
            Write-Host "    expected $sha256" -ForegroundColor Yellow
            Write-Host "    actual   $actual" -ForegroundColor Yellow
            return
        }

        Expand-Archive -LiteralPath $zip -DestinationPath $work -Force
        $extracted = Join-Path $work "cmigemo-default-win64"
        if (-not (Test-Path -LiteralPath $extracted -PathType Container)) {
            Write-Host "cmigemo archive layout unexpected (no cmigemo-default-win64/); skipping." -ForegroundColor Yellow
            return
        }

        # Flatten cmigemo-default-win64/ into ~/.local/share/cmigemo/ by copying
        # the extracted tree onto that exact path. Rebuild from scratch so a
        # partial previous install does not leave stale files behind.
        Ensure-RealDirectory (Join-Path $HOME ".local\share")
        if (Test-Path -LiteralPath $shareDir) {
            Remove-Item -LiteralPath $shareDir -Recurse -Force
        }
        Copy-Item -LiteralPath $extracted -Destination $shareDir -Recurse -Force

        Ensure-RealDirectory (Join-Path $HOME ".local\bin")
        if (Test-Path -LiteralPath $binLink) {
            Remove-Item -LiteralPath $binLink -Force
        }
        try {
            New-Item -ItemType SymbolicLink -Path $binLink -Target $exe -Force -ErrorAction Stop | Out-Null
            Write-Host "Linked: $binLink -> $exe"
        } catch {
            Write-Host "Failed to create symlink: $binLink -> $exe" -ForegroundColor Yellow
            Write-Host "Windows may require Developer Mode or Admin rights for symlinks." -ForegroundColor Yellow
        }
    } catch {
        Write-Host "cmigemo install failed: $($_.Exception.Message)" -ForegroundColor Yellow
    } finally {
        Remove-Item -LiteralPath $work -Recurse -Force -ErrorAction SilentlyContinue
    }

    Write-PrintLine $leftMessage "Finished."
}

function Setup-DevPackages {
    # Packages written here rather than merely used: point straight's clone at
    # the working tree in ~/Developer, so editing a package and restarting Emacs
    # is enough to see the change.  Without this the loaded copy is straight's
    # own clone, and every experiment costs a commit, a push and a pull.
    #
    # Skipped silently where the working tree is absent -- a machine that only
    # uses these packages should let straight clone them as usual.  A clone that
    # has anything of its own in it is left alone and reported: work done in the
    # wrong directory is still work, and this is not the place to discover it is
    # gone.
    $leftMessage = "Linking developed packages"
    Write-PrintLine $leftMessage "Started."

    $repos = Join-Path $HOME ".emacs.d\straight\repos"
    $build = Join-Path $HOME ".emacs.d\straight\build"
    $packages = @(
        "org-foresight",
        "org-calsync",
        "org-convect",
        "org-dayflow",
        "org-truss",
        "org-moor",
        "sill",
        "gensho-theme",
        "rustcity-theme"
    )

    foreach ($pkg in $packages) {
        $src = Join-Path $HOME "Developer\$pkg"
        $dest = Join-Path $repos $pkg

        if (-not (Test-Path -LiteralPath $src -PathType Container)) {
            continue
        }

        # A working tree is where the package is committed from, so its
        # commits go through the same hooks as this repository's.  straight's
        # own clones are never committed from and get none.
        Link-GitHooks $src

        $srcResolved = (Resolve-Path -LiteralPath $src).Path

        if (Test-Path -LiteralPath $dest) {
            $item = Get-Item -LiteralPath $dest -Force
            if ($item.LinkType -eq "SymbolicLink") {
                $current = $item.Target
                if ($current -is [array]) { $current = $current[0] }
                if (Test-SamePath $current $srcResolved) {
                    Write-Host "Skipping existing correct link: $dest"
                    continue
                }
                Write-Host "Replacing link: $dest"
                Remove-Item -LiteralPath $dest -Force
            } elseif ($item.PSIsContainer) {
                $gitDir = Join-Path $dest ".git"
                if (Test-Path -LiteralPath $gitDir) {
                    $porcelain = git -C $dest status --porcelain 2>$null
                    if ($porcelain) {
                        Write-Host "Skipping ${pkg}: straight's clone has uncommitted changes" -ForegroundColor Yellow
                        continue
                    }
                    $unpushed = git -C $dest log --oneline "@{u}..HEAD" 2>$null
                    if ($unpushed) {
                        Write-Host "Skipping ${pkg}: straight's clone has unpushed commits" -ForegroundColor Yellow
                        continue
                    }
                }
                Remove-Item -LiteralPath $dest -Recurse -Force
            } else {
                Remove-Item -LiteralPath $dest -Force
            }
        }

        New-Item -ItemType SymbolicLink -Path $dest -Target $srcResolved -Force | Out-Null
        $buildDir = Join-Path $build $pkg
        if (Test-Path -LiteralPath $buildDir) {
            Remove-Item -LiteralPath $buildDir -Recurse -Force
        }
        Write-Host "Linked: $dest -> $srcResolved"
    }

    Write-PrintLine $leftMessage "Finished."
}

function Register-AhkStartupShortcut {
    # Place <name>.ahk.lnk in the current user's Startup folder, targeting
    # the .ahk file itself. AutoHotkey (Scoop) runs it through the file
    # association. The shortcut is a pointer, not a copy: git pull of the
    # source plus a Windows restart is what picks up a new version.
    param(
        [Parameter(Mandatory = $true)][string]$Target
    )

    if (-not (Test-Path -LiteralPath $Target -PathType Leaf)) {
        return
    }

    $startup = [System.Environment]::GetFolderPath("Startup")
    $ws = New-Object -ComObject WScript.Shell
    $lnkPath = Join-Path $startup ((Split-Path -Leaf $Target) + ".lnk")

    if (Test-Path -LiteralPath $lnkPath) {
        $existingTarget = $ws.CreateShortcut($lnkPath).TargetPath
        if (Test-SamePath $existingTarget $Target) {
            Write-Host "Skipping existing correct shortcut: $lnkPath"
            return
        }
        Write-Host "Replacing shortcut: $lnkPath"
    }

    $shortcut = $ws.CreateShortcut($lnkPath)
    $shortcut.TargetPath = $Target
    $shortcut.WorkingDirectory = Split-Path -Parent $Target
    $shortcut.Save()
    Write-Host "Created startup shortcut: $lnkPath -> $Target"
}

function Setup-StartupShortcuts {
    # Register AutoHotkey scripts to launch at login: etc/ahk/*.ahk in this
    # repo (remap-windows-keys and friends), pointed at by shortcut rather
    # than copied. Idempotent.
    $leftMessage = "Registering AHK startup shortcuts"
    Write-PrintLine $leftMessage "Started."

    $ahkDir = Join-Path (Join-Path $script:ScriptDir "etc") "ahk"
    if (Test-Path -LiteralPath $ahkDir -PathType Container) {
        foreach ($ahk in @(Get-ChildItem -LiteralPath $ahkDir -Filter "*.ahk" -File -ErrorAction SilentlyContinue)) {
            Register-AhkStartupShortcut $ahk.FullName
        }
    }

    Write-PrintLine $leftMessage "Finished."
}

function Get-EmacsClientW {
    # The program, not Scoop's shim for it: a shim is a console program, and
    # a protocol handler that is one opens a console window on every call.
    # The shim's .shim file names what it stands for, under apps\emacs\current,
    # a junction Scoop moves on every update, so the path outlives updates.
    $cmd = Get-Command emacsclientw.exe -CommandType Application -ErrorAction SilentlyContinue |
        Select-Object -First 1
    if (-not $cmd) { return $null }
    $shim = [IO.Path]::ChangeExtension($cmd.Source, ".shim")
    if (Test-Path -LiteralPath $shim) {
        $line = Get-Content -LiteralPath $shim | Where-Object { $_ -match '^\s*path\s*=' } | Select-Object -First 1
        if ($line -match '=\s*"?([^"]+?)"?\s*$' -and (Test-Path -LiteralPath $Matches[1])) {
            return $Matches[1]
        }
    }
    return $cmd.Source
}

function Setup-OrgProtocol {
    # Register org-protocol:// for this user, pointing at emacsclientw, so a
    # browser's bookmarklet reaches Emacs.  HKCU rather than HKCR: no
    # administrator, and the path is found rather than written by hand.  The
    # hotkey that takes the document in front (org-moor) does not need this;
    # it calls emacsclientw itself.  Idempotent.
    $leftMessage = "Registering org-protocol"
    Write-PrintLine $leftMessage "Started."

    $client = Get-EmacsClientW
    if (-not $client) {
        Write-Host "emacsclientw.exe not found on PATH; skipping." -ForegroundColor Yellow
        Write-PrintLine $leftMessage "Finished."
        return
    }

    $root = "HKCU:\Software\Classes\org-protocol"
    New-Item -Path "$root\shell\open\command" -Force | Out-Null
    Set-ItemProperty -Path $root -Name "(default)" -Value "URL:Org Protocol"
    Set-ItemProperty -Path $root -Name "URL Protocol" -Value ""
    Set-ItemProperty -Path "$root\shell\open\command" -Name "(default)" `
        -Value ('"{0}" -n "%1"' -f $client)
    Write-Host "org-protocol -> $client"

    Write-PrintLine $leftMessage "Finished."
}

function Remove-StartupShortcuts {
    # Remove only the Startup-folder .lnk shortcuts that point back into this
    # repository (the AHK shortcuts created by Setup-StartupShortcuts). Shortcuts
    # placed by other apps (Flow Launcher, ...) are left
    # alone. Mirrors Is-ManagedLink's "managed = points into the repo" test, done
    # for .lnk shortcuts (the symlink helpers assume a reparse point).
    $startup = [System.Environment]::GetFolderPath("Startup")
    if (-not (Test-Path -LiteralPath $startup -PathType Container)) {
        return
    }

    $repo = Get-CanonicalPath $script:ScriptDir
    $ws = New-Object -ComObject WScript.Shell
    $removed = 0

    foreach ($lnk in (Get-ChildItem -LiteralPath $startup -Filter "*.lnk" -File -ErrorAction SilentlyContinue)) {
        $target = $ws.CreateShortcut($lnk.FullName).TargetPath
        if (-not $target) {
            continue
        }

        $resolved = Get-CanonicalPath $target
        if ($resolved.StartsWith($repo, [System.StringComparison]::OrdinalIgnoreCase)) {
            Write-Host "Removing managed startup shortcut: $($lnk.FullName) -> $target"
            Remove-Item -LiteralPath $lnk.FullName -Force
            $removed++
        }
    }

    if ($removed -eq 0) {
        Write-Host "  (no managed startup shortcuts found)"
    }
}

function Perform-FullBootstrap {
    $leftMessage = "Full bootstrap"
    Write-PrintLine $leftMessage "Started."

    Install-Scoop
    Set-UserEnvironment
    Install-ScoopPackages
    Install-Fonts
    Setup-Links
    Setup-DevPackages
    Install-Cmigemo
    Setup-StartupShortcuts
    Setup-OrgProtocol

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
