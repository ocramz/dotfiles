;; whitespace - because it's evil

(setq-default
 column-number-mode t
 indicate-empty-lines t
 truncate-lines t
 indent-tabs-mode nil
 tab-width 2
 whitespace-action '(auto-cleanup)
 whitespace-style '(empty face indentation lines-tail tabs trailing))

;; (add-hook-to-modes code-modes 'whitespace-mode)
