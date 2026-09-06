# My own Org packages, on these machines

The packages themselves say nothing about this setup, on purpose: they are
written to work on anybody's machine, and a README that describes mine is a
README that lies to everybody else. What is true only here lives in this
repository — mostly in `emacs.d/modules/my-app-org*.el` and `my-app-calendar.el`,
and the parts that are not Emacs configuration live here.

| Package | What it answers |
|---|---|
| [org-foresight](https://github.com/yoshzucker/org-foresight) | *When* does a thing fit on the day |
| [org-convect](https://github.com/yoshzucker/org-convect) | Purpose down the ladder of horizons, and the reduction back up |
| [org-upwell](https://github.com/yoshzucker/org-upwell) | Which files and URLs belong to the heading being lived |
| [org-calsync](https://github.com/yoshzucker/org-calsync) | The calendar, mirrored into Org |
| [org-dayflow](https://github.com/yoshzucker/org-dayflow) | The day-by-day timeline view |

## Loading them from the working tree

`bootstrap` (macOS/Linux) and `bootstrap.ps1`'s `Setup-DevPackages` (Windows)
point straight.el's clone at `~/Developer/<package>`, so editing a package and
restarting Emacs is enough to see the change. Without it the loaded copy is
straight's own clone and every experiment costs a commit, a push and a pull.

A machine that only *uses* these packages should skip that and let straight
clone from GitHub as usual — which is what happens automatically when
`~/Developer/<package>` is absent.

## org-upwell's watcher

The watcher is a login item, not an Emacs subprocess, and it is installed
per machine.

**Windows** is where it runs all day. `script/install-startup.ps1` puts a
Startup-folder shortcut pointing at `script/org-upwell-watch.ahk` *in the
package*, so `git pull` plus a log-off picks up a new version.
`bootstrap.ps1`'s `Setup-StartupShortcuts` registers the same path, so a
bootstrap is enough and the install script is only needed on its own.

Do **not** fold this into `etc/ahk/remap-windows-keys.ahk`. Reloading the
hotkey script would take the watcher down with it, and the two have nothing
to do with each other.

**macOS** is where it gets written and tried out.
`script/install-launchagent.macos` writes
`~/Library/LaunchAgents/org.upwell.watch.plist` and bootstraps the job. The
first capture asks for Automation permission; grant it to the job rather
than to Emacs.

Worth being honest about: the Windows side is exercised by using it, not by
a test suite. The ERT suites run on macOS, and the AutoHotkey and PowerShell
halves have no automated coverage at all. A change to those is a change to
be watched after, not one to assume.

## Settings that are mine, not the package's

`org-upwell-search-roots` and `org-upwell-create-directory` default to
something generic in the package. Here they name the actual trees — see the
`use-package org-upwell` block in `emacs.d/modules/my-app-org.el`.

`org-upwell-open-function` is `my/open-file`, so which extensions go to the
OS and which are read inside Emacs is decided once, in `my-files-ops.el`,
rather than twice.

## Keys and entry points

| Where | Key / command | What |
|---|---|---|
| Anywhere in Emacs | `C-c v` | `org-upwell-expand` |
| Agenda | `V` | the same, on the row |
| `C-c n v` | | `org-ql-view`, which used to be `C-c v` |
| Launcher / hotkey | `emacsclient -e "(org-upwell-expand-clock)"` | expand whatever is being clocked, from outside Emacs |
| Browser | bookmarklet in `script/org-upwell-bookmarklet.txt` | `org-protocol://upwell?url=…` |

## Where the data sits

`org-directory` is not the same place on the two machines and they do not sync
to each other — iCloud here, OneDrive there. That is deliberate; see the
commentary at the top of `emacs.d/modules/my-app-calendar.el` for what
crosses between them and what does not.

The watcher's traces are JSONL under `~/.local/share/org-upwell/` on both,
which is a path that exists without `.emacs.d` — the watcher starts at login,
often before Emacs.
