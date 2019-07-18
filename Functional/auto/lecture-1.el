(TeX-add-style-hook
 "lecture-1"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("subcaption" "labelformat=simple")))
   (TeX-run-style-hooks
    "latex2e"
    "beamer"
    "beamer10"
    "listings"
    "caption"
    "minted"
    "subcaption"))
 :latex)

