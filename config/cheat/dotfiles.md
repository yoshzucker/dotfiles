# dotfiles cheatsheet

`?` または `cheat` で表示。リッチ版は `Ctrl-G` (navi)。

## Keybindings (zsh)

| キー                | 機能                                         |
|---------------------|----------------------------------------------|
| `Ctrl-R`            | atuin: 履歴 fuzzy 検索                       |
| `Ctrl-T`            | fzf: ファイル選択 + bat preview              |
| `Alt-c`             | fzf: ディレクトリ jump + eza tree preview    |
| `Ctrl-G`            | navi: チートシート picker                    |
| `Ctrl-/`            | fzf 内: preview トグル                       |
| `Ctrl-y`            | fzf 内: 選択を pbcopy して終了               |
| `Alt-a` / `Alt-d`   | fzf 内: 全選択 / 全解除                      |
| `Tab`               | fzf-tab: 補完を fzf 化 + preview             |
| `Up`                | zsh prefix history search (atuin で潰さない) |
| `Ctrl-h` / `Ctrl-l` | backward / forward word                      |

## Aliases

- `ll` / `lla` — eza 詳細 (icons + git + age gradient)
- `lt` — eza tree (深さ 2, gitignore 尊重)
- `cat` — bat (`--paging=never`)
- `top` / `htop` — bottom (`btm`)
- `du` / `df` / `ps` — dust / duf / procs
- `lg` — lazygit
- `y` — yazi 起動、抜けたら cd 追従
- `z <q>` / `zi` — zoxide jump / picker
- `?` / `cheat` — このファイル
- `gitroot` — リポジトリルートへ cd

## Tools

| 用途        | コマンド                           |
|-------------|------------------------------------|
| ファイラ    | `yazi` (= `y`)                     |
| Git TUI     | `lazygit` (= `lg`)                 |
| GitHub      | `gh`                               |
| multiplexer | `zellij` (ローカル) / `tmux` (SSH) |
| 監視        | `btm`                              |
| viewer      | `bat`, `tldr`                      |
| diff        | `delta` (git pager に統合済み)     |
| ランタイム  | `mise`                             |
| JSON        | `jq`                               |

## Config 配置

`~/.config/{ripgrep,fd,eza,bat,bottom,atuin,zellij,sheldon,navi,cheat}` (dotfiles リポから symlink)
