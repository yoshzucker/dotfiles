# dotfiles

Modular, maintainable personal configuration files.

## Quick Start

**macOS**

```sh
git clone https://github.com/yoshzucker/dotfiles.git ~/dotfiles
~/dotfiles/bootstrap
```

**Windows (PowerShell 5.0+)**

```powershell
git clone https://github.com/yoshzucker/dotfiles.git $HOME\dotfiles
& $HOME\dotfiles\bootstrap.ps1
```

Without arguments, bootstrap installs what is missing — the package manager, the packages in `pkg/`, the font — and makes the links. It upgrades nothing. On the first Scoop install, Windows asks for a new terminal and a second run.

After it finishes, start a new shell (`exec $SHELL -l` on macOS, a new terminal on Windows) for PATH and the environment.

## Commands

| Command | What it does | When |
|---|---|---|
| `./bootstrap` | Install what is missing, then link | A fresh machine, or any time |
| `./bootstrap link` | Make or refresh the links, and remove those whose file is gone | After `git pull` |
| `./bootstrap update` | Upgrade every package in `pkg/` to its latest, then link | When an upgrade is wanted |
| `./bootstrap unlink` | Remove *only* the links into this repository | Before uninstalling |
| `./bootstrap doctor [--fix]` | Report (or remove) broken links | Diagnosing |

`.\bootstrap.ps1` takes the same commands. All are non-interactive and safe to re-run.

## After Pulling Changes

```sh
git pull
./bootstrap link        # macOS
.\bootstrap.ps1 link    # Windows
```

A pull changes configuration, not package versions. Emacs packages follow the lockfile (see [Package Versions](#package-versions)); Homebrew and Scoop packages stay where they are until `update` is run.

Homebrew and Scoop have no lockfile to keep: each installs its current version of a package, not a recorded one. So an upgrade is a command asked for by name, and nothing else — not a pull, not a plain `bootstrap` — runs one.

## How Symlinks Work (Safety First)

- `home/` → symlinked directly under `$HOME`
- `config/` → symlinked under `~/.config` (XDG)
- `local/bin/` → individual scripts symlinked into the real directory `~/.local/bin`
- `emacs.d/` → symlinked under `~/.emacs.d`
- `claude/skills/` → each skill linked into `~/.claude/skills`. The rest of `~/.claude` is Claude Code's own: `settings.json` is rewritten at runtime (model/theme/effortLevel), so tracking it only produces diff noise.

A directory in the repository becomes a real directory at the target, with its files linked one by one, so a program can write beside them.

**Conflict handling (XDG strict):**
When a real file (not a symlink) exists at a target path, it is moved to:
`${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles/backups/<timestamp>/<relative-path>`
Only real conflicting files are backed up. Existing symlinks (even if not ours) are replaced if they collide with our targets. No other data is ever touched.

**unlink / doctor safety:**
- `unlink` removes only symlinks whose target lies inside this repository.
- `link` and `doctor --fix` remove broken symlinks — ours whose file was deleted here, and anybody's that point nowhere.
- `$HOME` itself is read at its top level only; `~/.config`, `~/.local`, `~/.emacs.d` and `~/.claude/skills` all the way down, past generated directories (virtualenvs, `node_modules`, build and cache directories, straight's clones and builds, Emacs's state).

## Package Management (pkg/)

All package lists live under `pkg/` (intentionally **outside** `config/`, so they are never symlinked into `~/.config` or `~/.local`).

- `pkg/brew/Brewfile` — Homebrew (macOS) — `brew bundle`
- `pkg/scoop/scoopfile.json` — Scoop (Windows) — `scoop import`

**Why this layout?**
- Single source of truth per platform.
- `config/` stays pure user configuration (XDG-clean).
- Written by hand. A dump (`brew bundle dump`, `scoop export`) would take in whatever happens to be installed and drop the Brewfile's comments on why each entry is there, so a package is added by editing the file.

## Platform Notes

- **macOS**: Homebrew + emacs-plus (the `emacs-plus-app` cask installs Emacs.app into /Applications).
- **Windows**: Scoop (extras bucket included). Symlink support requires **Developer Mode** enabled (Settings → Update & Security → For developers) or running PowerShell as Administrator. First run after Scoop install usually requires terminal restart.
- **Emacs**: The `emacs.d/` tree in the repo is linked under `~/.emacs.d` by bootstrap.

## Package Versions

Every straight.el repository is pinned to a commit in `emacs.d/straight/versions/default.el`, the lockfile, which bootstrap links into `~/.emacs.d/straight/versions/`. One machine pulls packages forward and freezes the lockfile; every other machine only follows it.

Following is the default. A machine may pull only where an empty file, `~/.emacs.d/allow-straight-pull`, exists — created by hand and never tracked, so which machine pulls stays on that machine, and a new or rebuilt one follows until told otherwise.

- **Where pulling is allowed**: see [Updating](#updating). My own packages have to be pushed before the push of dotfiles, or the lockfile names commits GitHub does not have yet.
- **Followers**: after `git pull`, `M-x my/straight-apply-lockfile` puts every repository at the lockfile's commit; restart Emacs to rebuild what moved. Every command that would move a repository off the lockfile — `my/straight-update-lockfile`, `my/straight-pull-all` and straight's own pull, merge, normalize, push and freeze — refuses there, and a package the lockfile does not name is not cloned at all. A follower's start that stops on "not in the lockfile" means a package was added on the pulling machine without freezing.

straight itself is cloned by `init.el` at the lockfile's commit, not by straight's installer, which evaluates a file from a branch that moves. ghostel's native module and its Windows ConPTY files are kept only when their SHA-256 matches the one recorded in `emacs.d/modules/my-app-terminal.el`; when ghostel starts asking for a newer release, the download stops until that release's digests are added there.

## Updating

Look first, then move. Nothing upgrades on its own; each step below changes nothing until the last one, which the user runs.

**Emacs packages** (where pulling is allowed):

1. `M-x my/straight-update-lockfile` fetches every repository and writes into the lockfile the commit each would move to: the newest one at least seven days upstream (`C-u N` for another number), my own packages at their HEAD. No package moves. What would arrive is in `*straight-update-lockfile*` and `~/.emacs.d/var/straight-update-lockfile.txt`, beside the lockfile's diff.
2. Read it. Put back any line of the lockfile that should stay where it is.
3. `M-x my/straight-apply-lockfile` and restart Emacs: every repository moves to exactly the commit that was read, and nothing that arrived upstream since. The working trees linked from `~/Developer` are left alone.
4. Try it, run `./test/run`, and commit the lockfile. To go back instead: `git checkout emacs.d/straight/versions/default.el`, then `my/straight-apply-lockfile` again.

**Homebrew and Scoop**: `./bootstrap outdated` (`.\bootstrap.ps1 outdated`) lists installed and newer versions and changes nothing. Then `brew upgrade NAME` or `scoop update NAME` for the ones chosen, or `update` for all. Scoop can go back with `scoop reset NAME@VERSION` until `scoop cleanup`; Homebrew cannot go back, so the ones that matter are taken one at a time.

**With Claude**: the `update-review` skill (in `claude/skills/`) does the reading — runs the steps that change nothing, reads the commits and release notes, and returns a table of what changes, what is worth noticing, and whether to upgrade, wait or skip. It never upgrades. On a machine that only follows the lockfile it reviews the OS packages alone.

## My Own Packages

Ten packages are written here rather than merely used. Each has its own repository and its own README, which describe the package and say nothing about this machine.

- **Org**: org-foresight, org-convect, org-calsync, org-dayflow, org-truss, org-moor, org-item-tags
- **Themes**: gensho-theme, rustcity-theme
- **Emacs UI**: sill — one mode line at the foot of the frame instead of one per window

`bootstrap` links each of them from `~/Developer/<name>` into straight.el's repository directory, so what Emacs loads is the checkout being edited. A package left out of that list is cloned from GitHub instead, which succeeds quietly and then stops reflecting local edits — so a new package has to be added there as well as used.

A package dropped from the configuration leaves its clone behind, and after a few years of that the repository directory holds more of what is gone than of what is used. `M-x my/straight-prune-repos` sorts every directory there and shows the sort before it asks anything.

What counts as still wanted is every recipe the session registered — which is wider than what is written in a `use-package` form, because a dependency gets one too: `transient` is wanted because magit asks for it, and 45 of the repositories are wanted that way. Neither a `:if` that is false nor a package that has not loaded hides one, so what is left over really is what nothing asks for at first hand or second. The one thing that does hide them is a session that has not read the modules, so it refuses there rather than call everything abandoned.

Three kinds are never deleted: a link into `~/Developer`, straight itself, and any clone holding something uncommitted, stashed, or on a branch no remote has — asked of each candidate rather than assumed. What that leaves can be cloned again and lose nothing but the time, which is the one thing worth being asked about. `C-u` shows the listing without offering to delete.

`M-x my/straight-prune-builds` is the sibling, over the tree of built copies. It is the easier half: a build directory is links into the repository and the compiled files beside them, so nothing there is the only copy of anything and a mistake costs a rebuild. The one thing it cannot do is compare the two trees by name — a build directory is named by package and a repository by `:local-repo`, so `dash` is built from one called dash.el and `magit-section` from one called magit — which is why it asks the recipes instead.

Everything that is true only of *this* setup — how straight.el is pointed at `~/Developer`, which keys go where, and where the data sits — is in [`doc/org-packages.md`](doc/org-packages.md).

## Measuring It

Two profilers live in `emacs.d/`, neither loaded by `init.el`.

`profile-init.el` ranks a startup by file, with children excluded, so the
column names a culprit rather than a container. Run it in place of a normal
startup: `emacs -Q -l ~/dotfiles/emacs.d/profile-init.el`. Read the first row
with suspicion — advising `load` and `require` costs something, and it lands
on whichever frame was open first.

`profile-ops.el` ranks what happens afterwards: opening an Org file, building
an agenda, the scans the Org packages here do. Load it into a working Emacs
and run `M-x profile-ops`. It measures the real corpus and a generated copy of
it side by side, and reads the real files for their shape only — counts, never
content — so the report is safe to paste into a mail, which is how a result
gets off a machine that has no other way back.

Where the two shape columns disagree, `profile-ops-shape` is not yet saying
what the real files do; correcting it there is how the generated corpus comes
to stand in for one that cannot travel.

A second section measures what no corpus reaches — saving, splitting a window,
moving, `magit-status`, opening a capture — in one column, once, since none of
it grows with the number of Org files. Saving has two rows because it has two
numbers: the first save of a session is where whatever formats on save
arrives, and a mean taken across that reports a figure no save ever takes.

Neither is a regression test. A configuration that grows takes longer, and
that is not a fault. They are for the occasional look, to find the work
nothing asked for.

## Requirements

- macOS: bash, curl, git.
- Windows: PowerShell 5.0+, internet access. Developer Mode recommended for symlinks.
- All: 64-bit system, reasonable free disk space.

## Tests

```bash
./test/run                # every test/*-test.el
./test/run org-attach     # only files whose name contains this
```

A bare `emacs -Q --batch`, not the configuration: a test takes the
definitions it needs out of a module by name, so nothing has to be cloned
first. That means a test covers the functions and not the `use-package`
block that installs them; see the commentary in `test/my-test.el`.

## License

MIT License.

## Philosophy

Minimal, modular, long-term maintainable. XDG where it makes sense. Declarative packages. Idempotent everything. No surprises on re-run or fresh machines.
