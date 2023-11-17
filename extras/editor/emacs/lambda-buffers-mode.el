;;; lambda-buffers-mode.el --- SUMMARY -*- lexical-binding: t -*-

(require 'haskell-mode)

(define-derived-mode lambda-buffers-mode haskell-mode "lambda-buffers"
  "haskell-mode pretending to be lambda-buffers-mode"
  (haskell-indent-mode)
  (font-lock-add-keywords
   nil
   '(("\\<\\(record\\|sum\\|derive\\|opaque\\|prod\\)\\>" . 'font-lock-keyword-face)))
  (setq lsp-warn-no-matched-clients nil))

(add-to-list 'auto-mode-alist '("\\.lbf$" . lambda-buffers-mode))

(provide 'lambda-buffers-mode)
;;; lambda-buffers-mode.el ends here
