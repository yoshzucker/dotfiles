# =====================================================================
#  Cleanup-Desktop.ps1
#  会社設定で勝手に出てくるものを消す。
#    1) デスクトップの「ごみ箱以外」のシステムアイコンを隠す
#    2) タスクバーの Edge のピン止めを「恒久的に」外す
#       （再ピンの発生源 LayoutModification.xml から Edge を除去する）
#  管理者権限は不要（すべて自分のユーザー領域だけを変更）。
#
#  重要: このファイルは必ず UTF-8 (BOM付き) で保存すること。
#        BOM が無いと ja-JP の Windows PowerShell 5.1 が CP932 として読み、
#        日本語コメントが文字化けして構文エラーになり「一行も実行されない」。
# =====================================================================

$ErrorActionPreference = 'Continue'

# =====================================================================
#  1) デスクトップのシステムアイコンを隠す
# =====================================================================
#  値 1 = 隠す / 0 = 表示。ごみ箱だけ残したいので、ごみ箱は触らない。
$iconsToHide = @{
    '{20D04FE0-3AEA-1069-A2D8-08002B30309D}' = 'PC (This PC)'
    '{59031a47-3f72-44a7-89c5-5595fe6b30ee}' = 'ユーザーのファイル'
    '{F02C1A0D-BE21-4350-88B0-7367FC96EF3C}' = 'ネットワーク'
    '{5399E694-6CE5-4D6C-8FCE-1D8870FDCBA0}' = 'コントロールパネル'
}
#  ごみ箱 {645FF040-...} は残す（隠したくなったらここに足す）。

$hidePaths = @(
    'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\NewStartPanel',
    'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\HideDesktopIcons\ClassicStartMenu'
)

Write-Host '[1] デスクトップのシステムアイコンを隠す' -ForegroundColor Cyan
foreach ($p in $hidePaths) {
    if (-not (Test-Path $p)) { New-Item -Path $p -Force | Out-Null }
    foreach ($g in $iconsToHide.Keys) {
        New-ItemProperty -Path $p -Name $g -Value 1 -PropertyType DWord -Force | Out-Null
    }
}
foreach ($g in $iconsToHide.Keys) { Write-Host ("    隠す: {0}" -f $iconsToHide[$g]) }

# =====================================================================
#  2) タスクバーの Edge を恒久的に外す
# =====================================================================
#  診断で判明した事実:
#    - 再ピンの発生源は LayoutModification.xml（PinListPlacement="Replace"）。
#      explorer が再構築するたびに、この XML の通りに Edge を再ピンする。
#    - このファイルはユーザーの AppData 配下なので管理者権限なしで編集可能。
#  よって「XML から Edge を消す」のが恒久対策。あわせて現在のピンとキャッシュも掃除。
Write-Host '[2] タスクバーの Edge を恒久的に外す' -ForegroundColor Cyan

# --- 2a) 再ピンの発生源: LayoutModification.xml から Edge の行を除去 -----
function Remove-EdgeFromLayout {
    param([string]$Path)
    if (-not (Test-Path $Path)) { Write-Host ("    レイアウト無し: {0}" -f $Path); return }
    try {
        Copy-Item -LiteralPath $Path -Destination ($Path + '.bak') -Force -ErrorAction SilentlyContinue
        [xml]$xml = Get-Content -LiteralPath $Path -Raw
        $ns = New-Object System.Xml.XmlNamespaceManager($xml.NameTable)
        $ns.AddNamespace('t', 'http://schemas.microsoft.com/Start/2014/TaskbarLayout')
        $nodes = $xml.SelectNodes('//t:DesktopApp', $ns)
        $removed = 0
        foreach ($n in @($nodes)) {
            $lp = [string]$n.GetAttribute('DesktopApplicationLinkPath')
            if ($lp -match 'Edge' -or $lp -match 'msedge') {
                [void]$n.ParentNode.RemoveChild($n)
                $removed++
            }
        }
        if ($removed -gt 0) {
            $xml.Save($Path)
            Write-Host ("    レイアウトから Edge を {0} 件除去: {1}" -f $removed, $Path) -ForegroundColor Green
        } else {
            Write-Host ("    レイアウトに Edge 記述なし: {0}" -f $Path)
        }
    } catch {
        Write-Host ("    レイアウト編集エラー({0}): {1}" -f $Path, $_) -ForegroundColor Yellow
    }
}

# 実際に効くのは「今のユーザーの」レイアウト。Default プロファイル側は
# 新規ユーザー用で、書き込みに管理者権限が要ることが多いので試行のみ。
Remove-EdgeFromLayout -Path (Join-Path $env:LOCALAPPDATA 'Microsoft\Windows\Shell\LayoutModification.xml')
Remove-EdgeFromLayout -Path 'C:\Users\Default\AppData\Local\Microsoft\Windows\Shell\LayoutModification.xml'

# --- 2b) 現在ピンされている Edge の .lnk を削除（重複含む）--------------
$taskBarDir = Join-Path $env:APPDATA 'Microsoft\Internet Explorer\Quick Launch\User Pinned\TaskBar'
if (Test-Path $taskBarDir) {
    $edgeLnks = Get-ChildItem -LiteralPath $taskBarDir -Filter '*.lnk' -Force |
                Where-Object { $_.Name -match 'Edge' }
    if ($edgeLnks) {
        foreach ($lnk in $edgeLnks) {
            try {
                Remove-Item -LiteralPath $lnk.FullName -Force -ErrorAction Stop
                Write-Host ("    Edge の .lnk を削除: {0}" -f $lnk.Name)
            } catch { Write-Host ("    .lnk 削除失敗({0}): {1}" -f $lnk.Name, $_) -ForegroundColor Yellow }
        }
    } else {
        Write-Host '    ピン .lnk に Edge は無し'
    }
} else {
    Write-Host '    ピン止めフォルダが無い'
}

# --- 2c) ピンキャッシュを削除（explorer 再起動時に XML から作り直させる）-
#  Favorites を消すと explorer はレイアウト(2aで Edge を除去済み)から
#  タスクバーを再構築する。よって Edge は復活せず Outlook 等だけが残る。
$taskband = 'HKCU:\Software\Microsoft\Windows\CurrentVersion\Explorer\Taskband'
foreach ($val in 'Favorites', 'FavoritesResolve') {
    try {
        Remove-ItemProperty -Path $taskband -Name $val -ErrorAction Stop
        Write-Host ("    Taskband\{0} を削除" -f $val)
    } catch {}
}

# =====================================================================
#  3) explorer を再起動して反映
# =====================================================================
Write-Host '[3] explorer を再起動して反映' -ForegroundColor Cyan
Stop-Process -Name explorer -Force -ErrorAction SilentlyContinue
# explorer は Windows が自動で立ち上げ直す。

Write-Host '完了。' -ForegroundColor Green
