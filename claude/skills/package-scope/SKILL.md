---
name: package-scope
description: Keep a reusable package's own files free of the author's environment. Use when writing or editing a README, docstring, comment, default value, or test in a library/package/theme repository — anything published for other people to install — and before committing or publishing one. Catches personal paths, a configuration's function names, one machine's directory tree, third-party services only the author uses, and workflow facts (which OS runs it, which is "production") dressed up as the package's design. Also use when asked to review docs, prepare a release, or split package code from personal configuration.
---

# A package describes the package, not your machine

A credential scanner will not catch this. `gitleaks` passes, `leakseek`
passes, and the README still tells the world which of the author's two
laptops runs the thing, and which launcher they open it from.

The test is one question, asked of every sentence, default and identifier:

> **Would this still be true and useful for a stranger who installed this
> package and has never seen my setup?**

If no, it does not belong in the package. It belongs in the configuration
repository, which is allowed to be about one person.

## Where the line falls

| Belongs to the package | Belongs to the configuration |
|---|---|
| A *mechanism*: a defcustom, a hook, a function variable | The *value* put into it |
| "Open with the OS default application, or `pkg-open-function` if set" | `(setq pkg-open-function #'my/open-file)` |
| "Directories to look in when a path has gone" | `("~/Documents/project/" "~/Downloads/")` |
| Why a login item, not a subprocess | Which of my machines runs it all day |
| "From a launcher, a hotkey, or anything that runs a command" | "From FlowLauncher" |
| The integration a feature actually needs to name (Outlook, Calendar.app, org-mode) | A service that merely happens to be on my desk (a scanner's software, a launcher) |

The last row is the one worth slowing down on. Naming Outlook in a calendar
importer is a functional fact: the package drives Outlook. Naming a document
scanner in a *file-claiming* package is not — the package has never heard of
it, and there are a million such products. If the code does not reference it,
the docs should not either.

## The specific ways it gets in

1. **A configuration's function name in package code.**
   `(when (fboundp 'my/open-system) (my/open-system path))`, or a docstring
   that says "the thing `my/org-clock-obeys-the-row' exists to stop". Both
   are dead weight for everybody else, and the first one silently disables
   the package's own documented setting.
   → Take a function variable or run a hook. Let the configuration fill it.

2. **One machine's directories as hardcoded defaults.**
   → `defcustom`. A generic default (`~/Downloads/`) or none; the real trees
   go in the configuration.

3. **The author's workflow as the package's structure.**
   Headings like "Windows (production) / macOS (development)". The package
   does not have a production. Worse, it claims a testing standard that was
   never met.
   → Say what differs *technically* per OS. Say nothing about which one you
   personally use more.

4. **Personal vocabulary.** A private name for your notes directory reads as
   a term of art to a stranger who then goes looking for it.
   → "your Org files", "the store".

5. **A test that reads a file only you have.**
   `(skip-unless (file-readable-p "~/Documents/…/fixture.org"))` passes for
   one person and is skipped for everyone else, which is the same as not
   having the test.
   → Commit the fixture. Extract the *shapes* it covers, never the content.

6. **Cross-references to your other repositories as war stories.**
   "the same hole org-calsync hit" — a sibling package is fine to link to as
   a sibling; its incident history is not documentation.

7. **A colleague's name as sample data.**
   A `:DELEGATED_TO:` or `:PEOPLE:` property in a demo file reads as a
   placeholder to you and is a real person to them. Demo and test data want
   invented names — Robin, Alex — never one you would recognise from your
   own week. This is the failure that hides best: a common surname looks
   exactly like a placeholder, which is how it survives review.

## Doing the check

`check-package-scope` greps for the patterns that can be grepped, and is
wired into `pre-commit` for every repository that is not the configuration
one. Run it directly on a repository at any time:

```sh
check-package-scope [DIR]
```

Real names come from `~/.config/.package-scope.conf`, which stays on the
machine for the same reason leakseek's keyword file does: publishing the list
would publish the names. Add one when a new colleague turns up in your notes.

The check cannot judge rows 3, 4 and 6 above — those need reading. When you
have touched a README or a header comment, read the changed prose once with
the stranger's question in mind before committing.

## When something is worth keeping

Environment-specific knowledge is often genuinely useful — which install
step this machine needs, which key is bound where, what is honestly untested.
Move it, do not delete it. In this setup that is `dotfiles/doc/`, next to the
configuration that acts on it.
