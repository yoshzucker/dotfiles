# dotfiles cheatsheet

`?` または `cheat` で表示。

## Keybindings (zsh)

| キー                | 機能                                         |
|---------------------|----------------------------------------------|
| `Tab`               | zsh 補完メニュー (候補 2 以上で menu select) |
| `Ctrl-w e` / `Ctrl-w m` | ウィンドウサイズ循環 / 最大化 (Emacs と同じ。`Ctrl-w` は prefix 専用) |
| `Alt-Backspace`     | 単語削除                                     |
| `Ctrl-h` / `Ctrl-l` | backward / forward word                      |

## Aliases

- `?` / `cheat` — このファイル
- `gitroot` — リポジトリルートへ cd

## Tools

| 用途        | コマンド                           |
|-------------|------------------------------------|
| Git         | Emacs magit (メイン)               |
| multiplexer | `tmux`                             |

## Config 配置

`~/.config/{ripgrep,cheat}` (dotfiles リポから symlink)
