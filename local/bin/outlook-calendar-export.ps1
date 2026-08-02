<#
.SYNOPSIS
  Export the Outlook calendar to a local .ics for import into org (calendar.org).

.DESCRIPTION
  Reads the default Outlook calendar via COM -- entirely local, no cloud -- and
  writes an .ics of concrete occurrences in a rolling window.  In Emacs,
  `my/org-outlook-sync' runs this into a temporary file and imports it into
  calendar.org (idempotently by UID), so the meetings appear in org-agenda and
  org-dayflow alongside tasks and the child's schedule.

  Recurring meetings are expanded to one VEVENT per occurrence (IncludeRecurrences)
  so exceptions and cancellations are reflected accurately; each occurrence gets a
  stable UID (GlobalAppointmentID + start) so re-export upserts rather than
  duplicates. Windows + an installed Outlook are required.

.PARAMETER Days
  Days forward from today to export (default 30).

.PARAMETER Back
  Days backward from today to include (default 1).

.PARAMETER OutFile
  Output .ics path (default ~/Documents/memex/outlook.ics).  Normally supplied by
  `my/org-outlook-sync' as a temporary file.

.EXAMPLE
  .\outlook-calendar-export.ps1 -Days 45
#>
param(
    [int]$Days = 30,
    [int]$Back = 1,
    [string]$OutFile
)

# COM attaches to the running singleton Outlook, so this is Windows-only and must
# never call Quit() (that would close the user's live Outlook).
if ($env:OS -ne 'Windows_NT') {
    Write-Error 'outlook-calendar-export requires Windows with Outlook installed.'
    exit 1
}

if (-not $OutFile) {
    $OutFile = Join-Path $HOME 'Documents\memex\outlook.ics'
}
$dir = Split-Path -Parent $OutFile
if (-not (Test-Path $dir)) {
    New-Item -ItemType Directory -Path $dir -Force | Out-Null
}

$start = (Get-Date).Date.AddDays(-$Back)
$end   = (Get-Date).Date.AddDays($Days)

# RFC5545 TEXT escaping; the Emacs importer (my/org-ics--clean) reverses \, \; \n.
function Convert-IcsText([string]$s) {
    if ($null -eq $s) { return '' }
    $s = $s -replace '\\', '\\'
    $s = $s -replace ';', '\;'
    $s = $s -replace ',', '\,'
    $s = $s -replace "`r`n", '\n'
    $s = $s -replace "`n", '\n'
    return $s
}

$outlook = New-Object -ComObject Outlook.Application
try {
    $ns  = $outlook.GetNamespace('MAPI')
    $cal = $ns.GetDefaultFolder(9)   # olFolderCalendar
    $items = $cal.Items
    # Must Sort before Restrict, and Restrict is required to bound expansion of
    # infinitely-recurring events.
    $items.IncludeRecurrences = $true
    $items.Sort('[Start]')
    # 'g' = current-culture short date + time, the form Outlook's Restrict expects.
    # If Restrict errors under a different locale, adjust this date format.
    $filter = "[Start] >= '" + $start.ToString('g') + "' AND [Start] < '" + $end.ToString('g') + "'"
    $restricted = $items.Restrict($filter)

    $sb = New-Object System.Text.StringBuilder
    [void]$sb.Append("BEGIN:VCALENDAR`r`nVERSION:2.0`r`nPRODID:-//dotfiles//outlook-export//EN`r`n")
    $stamp = (Get-Date).ToUniversalTime().ToString('yyyyMMddTHHmmssZ')
    $count = 0

    foreach ($appt in $restricted) {
        try {
            $uid = $appt.GlobalAppointmentID + '-' + $appt.Start.ToString('yyyyMMddTHHmmss')
            [void]$sb.Append("BEGIN:VEVENT`r`n")
            [void]$sb.Append("UID:$uid`r`n")
            [void]$sb.Append("DTSTAMP:$stamp`r`n")
            if ($appt.AllDayEvent) {
                # Outlook's all-day End is already the exclusive next-day midnight,
                # matching the ICS all-day DTEND convention.
                [void]$sb.Append("DTSTART;VALUE=DATE:" + $appt.Start.ToString('yyyyMMdd') + "`r`n")
                [void]$sb.Append("DTEND;VALUE=DATE:"   + $appt.End.ToString('yyyyMMdd')   + "`r`n")
            } else {
                # Floating local time; the importer decodes it as local (company PC = JST).
                [void]$sb.Append("DTSTART:" + $appt.Start.ToString('yyyyMMddTHHmmss') + "`r`n")
                [void]$sb.Append("DTEND:"   + $appt.End.ToString('yyyyMMddTHHmmss')   + "`r`n")
            }
            [void]$sb.Append("SUMMARY:" + (Convert-IcsText $appt.Subject) + "`r`n")
            if ($appt.Location) {
                [void]$sb.Append("LOCATION:" + (Convert-IcsText $appt.Location) + "`r`n")
            }
            [void]$sb.Append("END:VEVENT`r`n")
            $count++
        } catch {
            Write-Warning ('Skipped a calendar item: ' + $_.Exception.Message)
        }
    }
    [void]$sb.Append("END:VCALENDAR`r`n")

    # UTF-8 without BOM (Japanese subjects; clean parse in Emacs).
    $utf8 = New-Object System.Text.UTF8Encoding($false)
    [System.IO.File]::WriteAllText($OutFile, $sb.ToString(), $utf8)
    Write-Output ("$OutFile ($count events)")
} finally {
    # Do NOT Quit(): $outlook is the user's running Outlook. Only release the ref.
    if ($outlook) {
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($outlook) | Out-Null
    }
    [System.GC]::Collect()
}
