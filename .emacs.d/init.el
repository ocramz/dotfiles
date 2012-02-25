;; no decorations plz

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; backups

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 24
      kept-old-versions 12
      version-control t)

;; emacsclient/server

(defun halt ()
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; useful fun(s)

(defun add-hook-to-modes (modes hook)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) hook)))

;; ido

(require 'ido)

(ido-mode t)

(setq ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ido-enable-prefix nil
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

;; y-is-yes/n-is-no

(defalias 'yes-or-no-p 'y-or-n-p)

;; org-mode

(defun org-agenda-toggle-visible ()
  "Toggle the visibility of blocked items"
  (interactive)
  (setq org-agenda-dim-blocked-tasks
        (if (eq org-agenda-dim-blocked-tasks nil)
            'invisible nil)))

(defun my-org-mode-hook ()
  (local-set-key (kbd "C-c s") 'org-sort)
  (local-set-key (kbd "C-c b") 'org-ido-switchb)
  (local-set-key (kbd "C-c v") 'org-agenda-toggle-visible))

(add-hook 'org-mode-hook 'my-org-mode-hook)

(setq org-agenda-custom-commands
      '(("vc" "View @COMPUTER" tags "+TODO=\"TODO\"+\@COMPUTER" nil)
        ("ve" "View @ERRAND"   tags "+TODO=\"TODO\"+\@ERRAND"   nil)
        ("vp" "View @PHONE"    tags "+TODO=\"TODO\"+\@PHONE"    nil)
        ("vr" "View @READ"     tags "+TODO=\"TODO\"+\@READ"     nil)
        ("vs" "View @SHOP"     tags "+TODO=\"TODO\"+\@SHOP"     nil)
        ("vw" "View @WORK"     tags "+TODO=\"TODO\"+\@WORK"     nil))
      org-stuck-projects '("+TODO=\"PROJ\"" ("TODO") nil "")
      org-tag-alist '(("@COMPUTER" . ?c)
                      ("@ERRAND"   . ?e)
                      ("@HOME"     . ?h)
                      ("@PHONE"    . ?p)
                      ("@READ"     . ?r)
                      ("@SHOP"     . ?s)
                      ("@WORK"     . ?w))
      org-todo-keywords '((sequence "PROJ(p!)" "TODO(t!)"
                                    "|"
                                    "DONE(d!)")
                          (sequence "|"
                                    "SKIP(s@)")))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)

;; deft

(defun after-deft ()
  (global-set-key (kbd "C-<f11>") 'deft-new-file)
  (global-set-key (kbd "C-<f12>") 'deft))

;; erc

(setq erc-nick "dysinger"
      erc-prompt-for-channel-key t
      erc-server "irc.freenode.net"
      erc-user-full-name "Tim Dysinger")

;; yasnippet

(defun after-yasnippet ()
  (require 'yasnippet)
  (add-to-list 'yas/snippet-dirs "~/.emacs.d/snippets")
  (yas/global-mode))

;; magit

(defun after-magit ()
  (global-set-key (kbd "C-x g") 'magit-status))

;; coding

(setq lisp-modes '(clojure
                   emacs-lisp
                   lfe
                   scheme)
      code-modes (apply #'append
                        (list lisp-modes
                              '(erlang
                                haskell
                                perl
                                python
                                ruby
                                sh
                                vhdl))))

;; lisp modes

(defun my-lisp-mode-hook ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(add-hook-to-modes lisp-modes 'my-lisp-mode-hook)

;; paredit

(defun my-paredit-mode-hook ()
  (show-paren-mode t)
  (paredit-mode t)
  (local-set-key (kbd "C-c (") 'paredit-backward-slurp-sexp)
  (local-set-key (kbd "C-c )") 'paredit-forward-slurp-sexp)
  (local-set-key (kbd "C-c 9") 'paredit-backward-barf-sexp)
  (local-set-key (kbd "C-c 0") 'paredit-forward-barf-sexp))

(defun after-paredit ()
  (add-hook-to-modes lisp-modes 'my-paredit-mode-hook))

;; code modes

(defun buffer-cleanup ()
  "Clean up the buffer"
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (delete-trailing-whitespace))

(defun my-code-mode-hook ()
  (local-set-key (kbd "C-m")   'newline-and-indent)
  (local-set-key (kbd "C-c r") 'align-regexp)
  (local-set-key (kbd "C-c n") 'buffer-cleanup))

(add-hook-to-modes code-modes 'my-code-mode-hook)

;; whitespace

(defun my-whitespace-mode-hook ()
  (setq whitespace-action '(auto-cleanup)
        whitespace-style  '(face tabs trailing lines-tail indentation empty))
  (whitespace-mode))

(add-hook-to-modes code-modes 'my-whitespace-mode-hook)

;; fic-ext-mode (FIXME/TODO highlighting)

(defun my-fic-ext-mode-hook ()
  (fic-ext-mode t))

(defun after-fic-ext-mode ()
  (add-hook-to-modes code-modes 'my-fic-ext-mode-hook))

;; flymake

(defun my-flymake-mode-hook ()
  (local-set-key (kbd "C-c e") 'flymake-goto-next-error))

(add-hook 'flymake-mode-hook 'my-flymake-mode-hook)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; erlang

(defun my-erlang-mode-hook ()
  (require 'erlang-flymake)
  (erlang-flymake-only-on-save))

(defun after-erlang ()
  (add-to-list 'load-path "~/.emacs.d/el-get/erlang/lib/tools/emacs")
  (require 'erlang-start)
  (add-hook 'erlang-mode-hook 'my-erlang-mode-hook))

(defun after-lfe ()
  (add-to-list 'load-path "~/.emacs.d/el-get/lfe/emacs")
  (require 'lfe-start))

;; haskell

(defun my-haskell-mode-hook ()
  (require 'inf-haskell-send-cmd))

(defun after-haskell-mode ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (setq haskell-program-name "cabal-dev ghci"))

(defun my-haskell-mode-exts-hook
  (require 'haskell-align-imports)
  (require 'haskell-sort-imports))

(defun after-haskell-mode-exts ()
  (add-hook 'haskell-mode-exts-hook 'my-haskell-mode-exts-hook))

;; julia

(defun after-julia-mode ()
  (add-to-list 'load-path "~/.emacs.d/el-get/julia-mode/contrib")
  (require 'julia-mode)
  (autoload 'julia-mode "julia" "Julia Mode")
  (add-to-list 'auto-mode-alist '("\\.j\\'" . julia-mode)))

;; el-get

(require 'whitespace) ;; el-get bombs with '"Invalid face" whitespace-line'
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (end-of-buffer)
      (eval-print-last-sexp))))

(setq
 el-get-bzr-shallow-clone t
 el-get-sources
 '((:name clojure-mode)
   (:name deft :after (lambda () (after-deft)))
   (:name el-get)
   (:name epresent)
   (:name eredis)
   (:name erlang :after (lambda () (after-erlang))
          :type git :url "https://github.com/erlang/otp.git")
   (:name fic-ext-mode :after (lambda () (after-fic-ext-mode)))
   (:name geiser)
   (:name gist)
   (:name graphviz-dot-mode)
   (:name haskell-mode :after (lambda () (after-haskell-mode)))
   (:name haskell-mode-exts :after (lambda () (after-haskell-mode-exts)))
   (:name lfe :after (lambda () (after-lfe))
          :type git :url "https://github.com/rvirding/lfe.git")
   (:name julia-mode :after (lambda () (after-julia-mode))
          :type git :url "https://github.com/JuliaLang/julia.git")
   (:name magit :after (lambda () (after-magit)))
   (:name magithub)
   (:name org-jira :type git :url "https://github.com/baohaojun/org-jira.git")
   (:name paredit :after (lambda () (after-paredit)))
   (:name vhdl-mode
          :type http-zip
          :url "http://www.iis.ee.ethz.ch/~zimmi/emacs/vhdl-mode-3.33.28.zip")
   (:name yasnippet :after (lambda () (after-yasnippet))
          :type git :url "https://github.com/capitaomorte/yasnippet.git")))

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

;; customizations

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'noerror)
