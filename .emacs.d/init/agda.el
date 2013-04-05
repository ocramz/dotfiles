;; Agda (Programming Language) I'm not using el-get here because I
;; don't have the time to figure out Agda's Emacs setup.  For now
;; we'll just do it like they suggest which is load from local disk.

(when (executable-find "agda-mode")
  (load-file
   (let ((coding-system-for-read 'utf-8))
     (shell-command-to-string "agda-mode locate"))))
