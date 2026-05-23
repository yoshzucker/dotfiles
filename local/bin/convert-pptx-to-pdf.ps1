param([string]$InputFile)
 
if (-not $InputFile -or -not (Test-Path $InputFile)) {
    Write-Output "ERROR: File not found: $InputFile"
    exit 1
}
if (-not ($InputFile -match '\.pptx$')) {
    Write-Output "ERROR: Please specify only pptx files"
    exit 1
}
 
$InputFile = (Resolve-Path $InputFile).Path
$pdfPath = [IO.Path]::ChangeExtension($InputFile, ".pdf")
$powerpoint = New-Object -ComObject PowerPoint.Application
 
try {
    $pres = $powerpoint.Presentations.Open($InputFile, $true, $false, $false)
    $pres.SaveAs($pdfPath, 32)
    $pres.Close()
    Write-Output $pdfPath
} finally {
    if ($pres) {
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($pres) | Out-Null
    }
    $powerpoint.Quit()
    [System.Runtime.InteropServices.Marshal]::ReleaseComObject($powerpoint) | Out-Null
    [System.GC]::Collect()
}
