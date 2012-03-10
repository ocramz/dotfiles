;; no decorations plz
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)

;; backups

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 128
      kept-old-versions 128
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

;; shortcuts

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

(require 'uniquify)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun after-smex ()
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'execute-extended-command))

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
      org-agenda-dim-blocked-tasks 'invisible
      org-agenda-files '("~/org")
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-done t
      org-agenda-todo-ignore-deadlines 'far
      org-agenda-todo-ignore-scheduled 'all
      org-archive-location "~/.emacs.d/org-archive/%s::"
      org-capture-templates '(("t" "Todo" entry
                               (file "~/org/inbox.org")
                               "* TODO %?" CAPTURED %u))
      org-clock-idle-time 15
      org-completion-use-ido t
      org-default-notes-file "~/org/inbox.org"
      org-directory "~/org"
      org-enable-priority-commands nil
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t
      org-export-latex-tables-centered t
      org-hide-leading-stars t
      org-mobile-directory "~/Dropbox/MobileOrg"
      org-mobile-inbox-for-pull "~/org/from-mobile.org"
      org-mobile-use-encryption t
      org-log-done 'time
      org-log-into-drawer t
      org-log-repeat 'time
      org-modules '(org-bbdb
                    org-bibtex
                    org-crypt
                    org-docview
                    org-gnus
                    org-info
                    org-jsinfo
                    org-habit
                    org-irc
                    org-mew
                    org-mhe
                    org-rmail
                    org-vm
                    org-wl
                    org-w3m)
      org-odd-levels-only t
      org-outline-path-complete-in-steps nil
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-refile-targets '((nil :maxlevel . 3)
                           (org-agenda-files :maxlevel . 1))
      org-return-follows-link t
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-src-window-setup 'current-window
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
                                    "SKIP(s@)"))
      org-todo-repeat-to-state "TODO"
      org-use-sub-superscripts '{})

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)

;; deft - full text search on quick/cheap free-form notes

(defun after-deft ()
  (setq deft-directory org-directory
        deft-extension "org"
        deft-text-mode 'org-mode
        deft-use-filename-as-title t)
  (global-set-key (kbd "C-x z") 'deft)
  (global-set-key (kbd "C-x Z") 'deft-new-file))

;; erc - irc

(require 'notifications)

(defun erc-global-notify (match-type nick message)
  (notifications-notify :title nick :body message :urgency 'low))

(add-hook 'erc-text-matched-hook 'erc-global-notify)

;; yasnippet - template expansion system ftw

(defun after-yasnippet ()
  (require 'yasnippet)
  (add-to-list 'yas/snippet-dirs "~/.emacs.d/snippets")
  (yas/global-mode))

;; magit - git mode for awesome commits

(defun after-magit ()
  (add-hook 'magit-mode-hook 'turn-on-magit-topgit)
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
                                julia
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

;; paredit - cruise-control for lisp editing

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
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'buffer-cleanup)
(global-set-key (kbd "C-c r") 'align-regexp)

(defun my-code-mode-hook ()
  (local-set-key (kbd "C-m") 'newline-and-indent))

(add-hook-to-modes code-modes 'my-code-mode-hook)

;; whitespace - because it's evil

(setq column-number-mode t
      indicate-empty-lines t
      truncate-lines t)

(setq-default indent-tabs-mode nil)

(setq whitespace-action '(auto-cleanup)
      whitespace-style '(face tabs trailing lines-tail indentation empty))

(add-hook-to-modes code-modes 'whitespace-mode)

;; fic-ext-mode - FIXME/TODO highlighting

(defun my-fic-ext-mode-hook ()
  (fic-ext-mode t))

(defun after-fic-ext-mode ()
  (add-hook-to-modes code-modes 'my-fic-ext-mode-hook))

;; flymake - builds your codes when you save

(defun my-flymake-mode-hook ()
  (local-set-key (kbd "C-c e") 'flymake-goto-next-error))

(add-hook 'flymake-mode-hook 'my-flymake-mode-hook)
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; erlang

(defun my-erlang-mode-hook ()
  (require 'erlang-flymake)
  (erlang-flymake-only-on-save))

(defun after-erlang ()
  (require 'erlang-start)
  (add-hook 'erlang-mode-hook 'my-erlang-mode-hook))

(defun after-lfe ()
  (require 'lfe-start))

;; haskell

(defun my-haskell-mode-hook ()
  (require 'inf-haskell-send-cmd))

(defun after-haskell-mode ()
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
  (setq haskell-font-lock-symbols 'unicode)
  (setq haskell-program-name "cabal-dev ghci"))

(defun my-haskell-mode-exts-hook
  (require 'haskell-align-imports)
  (require 'haskell-sort-imports))

(defun after-haskell-mode-exts ()
  (add-hook 'haskell-mode-exts-hook 'my-haskell-mode-exts-hook))

;; julia

(defun after-julia ()
  (autoload 'julia-mode "julia" "Julia Mode")
  (add-to-list 'auto-mode-alist '("\\.j\\'" . julia-mode)))

;; el-get - "emacsops"

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
 el-get-git-shallow-clone t
 el-get-github-default-url-type 'git
 el-get-sources
 '((:name clojure-mode)
   (:name deft :after (progn (after-deft)))
   (:name el-get)
   (:name epresent)
   (:name eredis)
   (:name erlang
          :type github
          :pkgname "erlang/otp"
          :load-path ("lib/tools/emacs")
          :after (progn (after-erlang)))
   (:name fic-ext-mode :after (progn (after-fic-ext-mode)))
   (:name geiser)
   (:name gist)
   (:name go-mode)
   (:name graphviz-dot-mode)
   (:name haskell-mode :after (progn (after-haskell-mode)))
   (:name haskell-mode-exts :after (progn (after-haskell-mode-exts)))
   (:name lfe
          :type github
          :pkgname "rvirding/lfe"
          :load-path ("emacs")
          :after (progn (after-lfe)))
   (:name notify)
   (:name julia
          :type github
          :pkgname "JuliaLang/julia"
          :load-path ("contrib")
          :after (progn (after-julia)))
   (:name magit :after (progn (after-magit)))
   (:name magithub)
   (:name markdown-mode)
   (:name org-jira :type github :pkgname "baohaojun/org-jira")
   (:name paredit :after (progn (after-paredit)))
   (:name perspective)
   (:name protobuf-mode)
   (:name pastebin)
   (:name scala-mode)
   (:name sicp)
   (:name sclang
          :type git
          :url "git://supercollider.git.sourceforge.net/gitroot/supercollider/supercollider"
          :load-path ("editors/scel/el"))
   (:name smex :after (progn (after-smex)))
   (:name vhdl-mode
          :type http-zip
          :url "http://www.iis.ee.ethz.ch/~zimmi/emacs/vhdl-mode-3.33.28.zip")
   (:name yasnippet
          :type github
          :pkgname "capitaomorte/yasnippet"
          :after (progn (after-yasnippet)))))

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

;; customizations

(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'noerror)

;; custom.el contents:
;;   erc-nick
;;   erc-server
;;   erc-user-full-name
;;   org-agenda-tags-column
;;   org-crypt-key
;;   org-mobile-encryption-password
;;   org-tags-column
