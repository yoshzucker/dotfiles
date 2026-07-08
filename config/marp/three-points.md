---
marp: true
theme: custom
paginate: true
header: "タイトル"
# footer: "Footer text"
---

<style>
/* 本文はタグ無し。各行 *スローガン* : 説明 で書く。
   イタリック=大きめ、それ以外=小さめ、にCSSで整える */
section li    { font-size: .62em; line-height: 1.9; }
section li em { font-size: 1.7em; font-style: italic; }
.map          { font-size: .6em; margin-top: .8em; }

/* 3つの柱：左に大きいイタリックの見出し、右に小さい箇条書きを縦に並べる */
.principles      { display: flex; flex-direction: column; gap: .5em; margin-top: .3em; }
.principle       { display: flex; align-items: center; gap: 1em; }
.principle .lead { flex: 0 0 9em; font-size: 1.2em; font-style: italic; line-height: 1.3; }
.principle ul    { margin: 0; padding-left: 1em; text-align: left; }
.principle li    { font-size: .58em; line-height: 1.6; }
.principle li em { font-size: 1em; font-style: normal; font-weight: 600; }
.principles + .map { margin-top: 2.0em; }
</style>

<!-- _paginate: true -->
<!-- _class: lead -->

# タイトル
YYYY-MM-DD / 発表者名

<p class="map">サブテキスト ── スライド全体のコンテキストや前提を一言で補足する</p>

---

## 3つの柱

<div class="principles">

<div class="principle">
<div class="lead">1. 見出し A</div>
<ul>
<li>説明1</li>
<li>説明2</li>
<li>説明3</li>
</ul>
</div>

<div class="principle">
<div class="lead">2. 見出し B</div>
<ul>
<li>説明1</li>
<li>説明2</li>
<li>説明3</li>
</ul>
</div>

<div class="principle">
<div class="lead">3. 見出し C</div>
<ul>
<li>説明1</li>
<li>説明2</li>
<li>説明3</li>
</ul>
</div>

</div>

<p class="map">まとめ文 ── 対比や目指す方向性を一行で示す</p>


---

<!-- _class: lead -->

## ご清聴ありがとうございました
