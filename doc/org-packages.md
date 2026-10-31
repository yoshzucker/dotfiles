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

## Loading them from the working tree

`bootstrap` (macOS/Linux) and `bootstrap.ps1`'s `Setup-DevPackages` (Windows)
point straight.el's clone at `~/Developer/<package>`, so editing a package and
restarting Emacs is enough to see the change. Without it the loaded copy is
straight's own clone and every experiment costs a commit, a push and a pull.

A machine that only *uses* these packages should skip that and let straight
clone from GitHub as usual — which is what happens automatically when
`~/Developer/<package>` is absent.

## Where the data sits

`org-directory` is not the same place on the two machines and they do not sync
to each other — iCloud here, OneDrive there. That is deliberate; see the
commentary at the top of `emacs.d/modules/my-app-calendar.el` for what
crosses between them and what does not.

## org-moor

Not on GitHub yet: until it is, `~/Developer/org-moor` has to be linked into
straight (`setup_dev_packages`), or straight tries to clone a repository that
does not exist.

Copies go to `memex/source/`, the package default, so they travel with memex
and are never browsed by hand.  `:memex` is plain rg and skips `source/` and
the older attachments in `data/`; `:source` is rga over `source/`.  That split
is also what keeps rga's PDF and Office extraction from stalling `:memex` on
Windows.

The hotkey runs `script/org-moor-front.*` in straight's clone once and
exits; nothing new stays running.  On macOS the first use asks to let
Karabiner control the app in front; allow it.  On Windows the script calls
emacsclientw itself (Scoop's shim followed to the real program), so it
does not depend on org-protocol being registered.

The bookmarklet does.  `bootstrap.ps1` (`Setup-OrgProtocol`, in a full
bootstrap and in `update`) registers it for the user under HKCU with the
real emacsclientw path.

Web pages are kept with `monolith` (Brewfile, scoopfile).  Without it a page
is taken as its URL only.

| Where | Key / command | What |
|---|---|---|
| Anywhere in Emacs | `C-c m q` | quote the clipboard, titled; `C-u` picks the heading |
| | `C-c m f` | take a file (in Dired, the one at point): moved from Downloads and the desktop, copied from anywhere else; `C-u` picks the heading, `C-u C-u` also turns move and copy round |
| | `C-c m w` | take a web page |
| | `C-c m j` | back to anything taken, newest first |
| | `C-c m d` | in Dired, take the file at point first; then draft what the heading's source means; in the draft `C-c C-c` writes, `C-c C-k` discards, `C-c C-a` asks Claude Code to rewrite it |
| | `C-c m s` | sweep: Downloads and the desktop as one Dired buffer, newest first |
| | `C-c m p` / `C-c m P` | put the file at point in a directory under project/ or reference/ (picked or made); `P` also takes it, as a copy |
| | `C-c m n` | reading a copy (pdf-tools or otherwise): add this page and the selection to the source's draft |
| | `C-c m o` | open the source's copy; `C-u` its origin |
| | `C-c m x` | prune: copies largest first, `d` mark, `x` let go (heading, notes and origin stay) |
| ex line | `:source` | search the copies |
| Browser | bookmarklet in `script/org-moor-bookmarklet.txt` | `org-protocol://moor?url=…` |
| Anywhere (macOS) | `control+option+m` | take the document in front (Karabiner rule `org-moor-front`, enable it in Karabiner's settings) |
| Anywhere (Windows) | `Ctrl+Alt+M` | the same, from `etc/ahk/remap-windows-keys.ahk` |

A key after `C-c m` (or any prefix) that is slow in coming brings up
which-key's list; typed on, nothing shows.

org-protocol is loaded by the first emacsclient call that carries an
org-protocol URL, not at startup.  `C-c m` was Marp's in Markdown buffers;
Marp's commands are now reached by `M-x`.
