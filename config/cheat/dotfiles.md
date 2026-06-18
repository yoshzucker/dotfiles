# dotfiles cheatsheet

`?` または `cheat` で表示。

## Keybindings (zsh)

| キー                | 機能                                         |
|---------------------|----------------------------------------------|
| `Ctrl-R`            | fzf: 履歴 fuzzy 検索                         |
| `Ctrl-T`            | fzf: ファイル選択 + bat preview              |
| `Alt-c`             | fzf: ディレクトリ jump + eza tree preview    |
| `Ctrl-/`            | fzf 内: preview トグル                       |
| `Ctrl-y`            | fzf 内: 選択を pbcopy して終了               |
| `Alt-a` / `Alt-d`   | fzf 内: 全選択 / 全解除                      |
| `Tab`               | fzf-tab: 補完を fzf 化 + preview             |
| `Up`                | zsh prefix history search                    |
| `Ctrl-h` / `Ctrl-l` | backward / forward word                      |

## Aliases

- `ll` / `lla` — eza 詳細 (icons + git + age gradient)
- `lt` — eza tree (深さ 2, gitignore 尊重)
- `cat` — bat (`--paging=never`)
- `y` — yazi 起動、抜けたら cd 追従
- `z <q>` / `zi` — zoxide jump / picker
- `?` / `cheat` — このファイル
- `gitroot` — リポジトリルートへ cd

## Tools

| 用途        | コマンド                           |
|-------------|------------------------------------|
| ファイラ    | `yazi` (= `y`)                     |
| Git         | Emacs magit (メイン)               |
| multiplexer | `tmux`                             |
| viewer      | `bat`                              |
| diff        | `delta` (git pager に統合済み)     |

## Config 配置

`~/.config/{ripgrep,fd,eza,bat,cheat}` (dotfiles リポから symlink)
