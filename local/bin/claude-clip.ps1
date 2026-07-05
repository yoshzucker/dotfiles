<#
.SYNOPSIS
    Transforms clipboard (or stdin) text via the `claude` CLI using a named preset prompt.
.DESCRIPTION
    Reads a preset instruction from ~/.config/claude/prompts/<Preset>.md, reads the text
    (clipboard by default, or stdin with -Raw), runs `claude` in print mode with a fast
    model, and writes the result to the clipboard (default) or stdout (-Raw). UTF-8 is
    enforced on both pipe legs. On any failure the original clipboard is left untouched.
.PARAMETER Preset
    Name of a prompt file (without .md) under ~/.config/claude/prompts/.
    Any name is accepted as long as the file exists. Defaults to the
    CLAUDE_CLIP_PRESET environment variable (how the AHK front-end passes the
    choice, which avoids command-line encoding issues with non-ASCII names).
.PARAMETER Model
    claude model alias/name. Default 'haiku' (fast).
.PARAMETER Raw
    Read from stdin, print to stdout, do not touch the clipboard. For testing.
.EXAMPLE
    "Why? Explain the reason." | .\claude-clip.ps1 -Preset soften -Raw
.NOTES
    This script is kept ASCII-only. All Japanese lives in the UTF-8 preset files
    under ~/.config/claude/prompts/ (including the shared _guard.md), because
    Windows PowerShell 5.1 misreads a BOM-less UTF-8 .ps1 as the ANSI code page.
#>
[CmdletBinding()]
param(
    [Parameter(Position = 0)]
    [string]$Preset = $env:CLAUDE_CLIP_PRESET,
    [Parameter()][string]$Model = 'haiku',
    [Parameter()][switch]$Raw
)

$OutputEncoding           = [System.Text.UTF8Encoding]::new($false)
[Console]::OutputEncoding = [System.Text.UTF8Encoding]::new($false)
[Console]::InputEncoding  = [System.Text.UTF8Encoding]::new($false)
$ErrorActionPreference = 'Stop'

try {
    if ([string]::IsNullOrWhiteSpace($Preset)) { Write-Error "No preset given (pass -Preset or set CLAUDE_CLIP_PRESET)."; exit 1 }
    $promptDir = Join-Path $HOME ".config/claude/prompts"

    $guardPath = Join-Path $promptDir "_guard.md"
    if (-not (Test-Path -LiteralPath $guardPath)) { Write-Error "Guard file not found: $guardPath"; exit 1 }
    $Guard = (Get-Content -LiteralPath $guardPath -Raw -Encoding UTF8).Trim()

    $promptPath = Join-Path $promptDir "$Preset.md"
    if (-not (Test-Path -LiteralPath $promptPath)) { Write-Error "Preset file not found: $promptPath"; exit 1 }
    $instruction = (Get-Content -LiteralPath $promptPath -Raw -Encoding UTF8).Trim()

    # Resolve claude (AHK's powershell lacks ~/.local/bin on PATH)
    $claude = Get-Command claude -ErrorAction SilentlyContinue
    if (-not $claude) {
        $cand = Join-Path $HOME ".local/bin/claude.exe"
        $shim = Join-Path $HOME "scoop/shims/claude.exe"
        if     (Test-Path -LiteralPath $cand) { $claude = $cand }
        elseif (Test-Path -LiteralPath $shim) { $claude = $shim }
    }
    if (-not $claude) { Write-Error "claude CLI not found."; exit 1 }
    $claudeExe = if ($claude -is [System.Management.Automation.CommandInfo]) { $claude.Source } else { $claude }

    # -Raw reads stdin directly; $input does not capture piped input reliably when
    # a param() script is launched with -File (the input fails to bind and is lost).
    if ($Raw) { $text = [Console]::In.ReadToEnd() } else { $text = Get-Clipboard -Raw }
    if ([string]::IsNullOrWhiteSpace($text)) { Write-Warning "No text to transform."; exit 1 }

    # Guard + preset are combined into a single --system-prompt. This REPLACES
    # claude's default agent prompt, which is what keeps the output clean (no
    # persona/skills/CLAUDE.md pollution). Do NOT use --append-system-prompt
    # (it hangs here) and NOT --bare (it drops the login and fails auth).
    $systemPrompt = $Guard + "`n`n" + $instruction

    # Wrap the text in <input></input> so the model treats it purely as data to
    # transform, not as a message to answer. The guard references these tags.
    # Without this, first-person / request-like inputs (e.g. "もっと知りたい")
    # get a conversational reply instead of being rewritten.
    $wrapped = "<input>`n" + $text + "`n</input>"

    $result = $wrapped | & $claudeExe -p `
        --model $Model `
        --output-format text `
        --system-prompt $systemPrompt 2>$null
    $result = ($result | Out-String).Trim()

    # Defensive: strip the wrapper tags if the model echoed them.
    $result = ($result -replace '^\s*<input>\s*', '') -replace '\s*</input>\s*$', ''
    $result = $result.Trim()

    if ($LASTEXITCODE -ne 0)                   { Write-Error "claude exited $LASTEXITCODE."; exit 1 }
    if ([string]::IsNullOrWhiteSpace($result)) { Write-Error "Empty output; clipboard unchanged."; exit 1 }

    if ($Raw) { Write-Output $result } else { Set-Clipboard -Value $result }
    exit 0
}
catch { Write-Error "claude-clip failed: $_"; exit 1 }
