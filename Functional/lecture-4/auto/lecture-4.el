(TeX-add-style-hook
 "lecture-4"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("subcaption" "labelformat=simple")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "caption"
    "minted"
    "tikz"
    "xcolor"
    "subcaption"))
 :latex)

