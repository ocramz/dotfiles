(setq lisp-modes '(clojure
                   nrepl
                   emacs-lisp
                   scheme)
      code-modes (apply #'append
                        (list lisp-modes
                              '(erlang
                                haskell
                                python
                                ruby
                                sh
                                vhdl))))

(defun cleanup-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(global-set-key (kbd "C-c r") 'align-regexp)

(add-hook-to-modes
 code-modes
 (lambda ()
   (local-set-key (kbd "C-m") 'newline-and-indent)))
