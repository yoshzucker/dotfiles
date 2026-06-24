---
marp: true
theme: custom
paginate: true
# header: "Header text"
# footer: "Footer text"
---

<!-- _paginate: false -->
<!-- _class: lead -->

# プレゼンテーションタイトル

サブタイトル / 発表者名

---

## セクション見出し

- 項目1
- 項目2
- 項目3

---

## インデントとスペース制御

通常テキスト（インデントなし）

<div style="padding-left: 2em">2em インデント — padding-left で左余白を指定</div>

<div style="padding-left: 4em">4em インデント — より深い階層</div>

<br>

`<br>` で明示的な縦スペース挿入。段落間の余白に使う。

---

## テーブル

| 項目 | 担当 | 期限 |
|:-----|:----:|-----:|
| 設計 | 山田 | 6/30 |
| 実装 | 鈴木 | 7/15 |
| テスト | 佐藤 | 7/31 |

`|:---|` 左寄せ　`|:---:|` 中央　`|---:|` 右寄せ

---

<!-- _class: dense -->

## <!-- fit --> フォントサイズ縮小（dense クラス + fit 見出し）

`<!-- _class: dense -->` でスライド全体のフォントを縮小（テーマの `section.dense` が必要）。  
見出しには `## <!-- fit --> ...` を付けると幅に合わせて自動スケール（Marp 組み込み機能）。

- **dense クラス**: 本文が `0.75em` に縮小 — リスト・段落・表すべてに効く
- **fit 見出し**: Marp の Marpit auto-scaling。テーマに依存せず使える
- dense の縮小率を変えるには `section.dense { font-size: ...; }` を調整
- さらに小さくしたい場合は `section.compact { font-size: 0.65em; }` など追加も可
- Marp には body テキストの自動サイズ検出はないため、これが現実的な近似
- この行も dense のおかげでページに収まっている

---

<!-- _class: lead -->

## ご清聴ありがとうございました
