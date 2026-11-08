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
| [org-calsync](https://github.com/yoshzucker/org-calsync) | The calendar, mirrored into Org |
| [org-dayflow](https://github.com/yoshzucker/org-dayflow) | The day-by-day timeline view |
| [org-truss](https://github.com/yoshzucker/org-truss) | Whether an argument stands, and how a document is arranged around it |
| [org-moor](https://github.com/yoshzucker/org-moor) | What arrives in pieces, taken in, written up, and found again |
| [org-item-tags](https://github.com/yoshzucker/org-item-tags) | Tags on the items of a plain list, as a heading has them |

## Loading them from the working tree

`bootstrap` (macOS) and `bootstrap.ps1`'s `Setup-DevPackages` (Windows)
point straight.el's clone at `~/Developer/<package>`, so editing a package and
restarting Emacs is enough to see the change. Without it the loaded copy is
straight's own clone and every experiment costs a commit, a push and a pull.

A machine that only *uses* these packages should skip that and let straight
clone from GitHub, at the commit the lockfile names — which is what happens
automatically when `~/Developer/<package>` is absent.  Where the working tree
is linked, freezing records its HEAD, so a package is pushed before the
dotfiles commit that carries the lockfile.

## Where the data sits

`org-directory` is not the same place on the two machines and they do not sync
to each other — iCloud here, OneDrive there. That is deliberate; see the
commentary at the top of `emacs.d/modules/my-app-calendar.el` for what
crosses between them and what does not.

## org-moor

Copies go to `memex/sources/`, the package default, so they travel with memex
and are never browsed by hand.  `:rg` in memex searches them with the notes:
a source is found by the machine-readable copy org-moor keeps beside it.

The hotkey runs `script/org-moor-front.*` in straight's clone once and
exits; nothing new stays running.  On macOS the first use asks to let
Karabiner control the app in front; allow it.  On Windows the script calls
emacsclientw itself (Scoop's shim followed to the real program).

No `org-protocol://` handler is registered and org-protocol is never
loaded: a web page cannot hand Emacs anything.  A page is taken with the
hotkey, or with `C-c m w`, and the browser saves its copy.

| Where | Key / command | What |
|---|---|---|
| Anywhere in Emacs | `C-c m c` | take the clipboard |
| | `C-c m q` | quote the clipboard into the heading |
| | `C-c m f` | take a file (in Dired, the one at point): moved from Downloads and the desktop, copied from anywhere else; `C-u` picks the heading, `C-u C-u` also turns move and copy round |
| | `C-c m w` | take a web page; the browser saves its copy |
| | `C-c m j` | back to anything taken, newest first |
| | `C-c m s` | sweep: Downloads and the desktop as one Dired buffer, newest first |
| | `C-c m p` / `C-c m P` | put the file at point in a directory under project/ or reference/ (picked or made); `P` also takes it |
| | `C-c m n` | read the source beside its heading |
| | `C-c m i` | note this place in the source |
| | `C-c m o` / `C-c m O` | open the source's copy / its origin |
| | `C-c m y` | copy the origin to the clipboard |
| | `C-c m M` | open the machine-readable copy |
| | `C-c m t` | retitle the source |
| | `C-c m x` | prune: copies largest first, `d` mark, `x` let go (heading, notes and origin stay) |
| | `C-c m a` / `C-c m k` | the sources still waiting for a copy / keep a copy for those that have none |
| Anywhere (macOS) | `control+option+m` | take the document in front (Karabiner rule `org-moor-front`, enable it in Karabiner's settings) |
| Anywhere (Windows) | `Ctrl+Alt+M` | the same, from `etc/ahk/remap-windows-keys.ahk` |

A key after `C-c m` (or any prefix) that is slow in coming brings up
which-key's list; typed on, nothing shows.
