<#
.SYNOPSIS
    Saves an image from the clipboard to a specified PNG file.

.DESCRIPTION
    This script checks if the Windows clipboard contains an image and,
    if so, saves it to the specified output path in PNG format.

.PARAMETER OutputPath
    The full file path where the clipboard image should be saved.

.EXAMPLE
    .\save-clipboard-image.ps1 -OutputPath "C:\Users\me\Pictures\clip.png"
#>

[CmdletBinding()]
param (
    [Parameter(Mandatory = $true, Position = 0)]
    [string]$OutputPath
)

# Load required .NET type
Add-Type -AssemblyName System.Windows.Forms

# Get image from clipboard
$image = [System.Windows.Forms.Clipboard]::GetImage()

if ($null -ne $image) {
    try {
        $image.Save($OutputPath, [System.Drawing.Imaging.ImageFormat]::Png)
        Write-Output "Saved image to: $OutputPath"
    } catch {
        Write-Error "Failed to save image: $_"
    }
} else {
    Write-Warning "No image found in clipboard."
}
