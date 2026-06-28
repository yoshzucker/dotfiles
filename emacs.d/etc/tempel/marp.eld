;; Marp templates for markdown-mode (gfm-mode inherits via derived-mode-p).
;; Loaded through `tempel-path' (etc/tempel/*.eld).  Expand via Corfu
;; completion or tempel-abbrev: type the name, then TAB between fields.

markdown-mode

(deck "---" n "marp: true" n "theme: custom" n "paginate: true" n
      "# header: \"\"" n "# footer: \"\"" n "---" n n
      "<!-- _paginate: false -->" n "<!-- _class: lead -->" n n
      "# " p n n p n)
(slide "---" n n "## " p n n q)
(lead "<!-- _class: lead -->")
(nopage "<!-- _paginate: false -->")
(fs "<!-- _class: fs-" (p "80") " -->")
(fit "## <!-- fit --> " p)
(indent "<div style=\"padding-left: " (p "2") "em\">" p "</div>")
(table "| " p " | " p " |" n "|:--|--:|" n "| " p " | " p " |" n)

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
