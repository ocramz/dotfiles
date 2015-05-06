;; NO FRILLS

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)

;; CUSTOM FILE

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;; BACKUP FILES

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-directory-alist `((".*" . ,temporary-file-directory)))

;; ELPA PACKAGES

(setq package-archives
      '(("ELPA"           . "http://tromey.com/elpa/")
        ("SC"             . "http://joseito.republika.pl/sunrise-commander/")
        ("gnu"            . "http://elpa.gnu.org/packages/")
        ("marmalade"      . "http://marmalade-repo.org/packages/")
        ("melpa"          . "http://melpa.org/packages/")
        ("melpa-stable"   . "http://stable.melpa.org/packages/")
        ("org"            . "http://orgmode.org/elpa/"))
      package-pinned-packages
      '((arduino-mode        . "melpa")
        (company             . "melpa")
        (company-cabal       . "melpa")
        (company-ghc         . "melpa")
        (deft                . "melpa")
        (flycheck-rust       . "melpa")
        (flymake-rust        . "melpa")
        (gist                . "melpa")
        (gnuplot             . "melpa")
        (graphviz-dot-mode   . "melpa")
        (hamlet-mode         . "melpa")
        (haskell-emacs       . "melpa")
        (haskell-mode        . "melpa")
        (hindent             . "melpa")
        (ido-ubiquitous      . "melpa")
        (idris-mode          . "melpa")
        (js2-mode            . "melpa")
        (magit               . "melpa")
        (magit-gh-pulls      . "melpa")
        (markdown-mode       . "melpa")
        (nix-mode            . "melpa")
        (org                 . "org")
        (org-ac              . "melpa")
        (org-magit           . "melpa")
        (org-pandoc          . "melpa")
        (org-trello          . "melpa")
        (paredit             . "melpa")
        (rainbow-delimiters  . "melpa")
        (rainbow-identifiers . "melpa")
        (rust-mode           . "melpa")
        (smex                . "melpa")
        (textile-mode        . "melpa")
        (yaml-mode           . "melpa")
        (yasnippet           . "melpa"))
      el-get-user-packages '(ghc-mod))

(package-initialize t)

;; EL-GET PACKAGES

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path (locate-user-emacs-file "el-get-user/recipes"))

;; BOOTSTRAP

(defun bootstrap ()
  (interactive)
  (package-refresh-contents)
  (mapc (lambda (package)
          (let ((pkg (car package)))
            (unless (require pkg nil t)
              (package-install pkg))))
        package-pinned-packages)
  (el-get 'sync el-get-user-packages))

;; GENERAL

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'ido-mode)
(add-hook 'after-init-hook 'ido-yes-or-no-mode)
(add-hook 'after-init-hook 'yas-global-mode)

;; PROGRAMMING

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'show-paren-mode)
(add-hook 'prog-mode-hook 'whitespace-mode)

;; PAREDIT

(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; IDO

(setq ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess)

;; SMEX

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; WHITESPACE

(setq-default column-number-mode t
              indent-tabs-mode nil
              indicate-empty-lines t
              tab-width 4
              truncate-lines t
              whitespace-action '(auto-cleanup)
              whitespace-style '(empty indentation tabs trailing))

;; GIT

(global-set-key (kbd "C-x g") 'magit-status)

;; ORG-MODE

(setq-default
 org-default-notes-file "~/org/inbox.org"
 org-directory "~/org"
 org-enable-priority-commands nil
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
 org-export-latex-tables-centered t
 org-hide-leading-stars t
 org-log-done 'time
 org-log-into-drawer t
 org-log-redeadline 'time
 org-log-refile 'time
 org-log-repeat 'time
 org-log-reschedule 'time
 org-log-states-order-reversed nil
 org-mobile-directory "~/org/mobile/"
 org-mobile-inbox-for-pull "~/org/inbox.org"
 org-mobile-use-encryption t
 org-odd-levels-only t
 org-outline-path-complete-in-steps nil
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-use-outline-path 'file
 org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 1))
 org-return-follows-link t
 org-src-tab-acts-natively t
 org-src-fontify-natively t
 org-src-window-setup 'current-window
 org-use-sub-superscripts '{}
 org-agenda-custom-commands
 '(("vc" "@COMPUTER"
    tags "+TODO=\"TODO\"+\@COMPUTER"
    nil)
   ("ve" "@ERRAND"
    tags "+TODO=\"TODO\"+\@ERRAND"
    nil)
   ("vh" "@HOME"
    tags "+TODO=\"TODO\"+\@HOME"
    nil)
   ("vp" "@PHONE"
    tags "+TODO=\"TODO\"+\@PHONE"
    nil)
   ("vr" "@READ"
    tags "+TODO=\"TODO\"+\@READ"
    nil)
   ("vs" "@SHOP"
    tags "+TODO=\"TODO\"+\@SHOP"
    nil)
   ("va" "@WAIT"
    tags "+TODO=\"TODO\"+\@WAIT"
    nil)
   ("vw" "@WORK"
    tags "+TODO=\"TODO\"+\@WORK"
    nil)
   ("v-" "NO CONTEXT"
    tags "-@COMPUTER-@ERRAND-@HOME-@PHONE-@READ-@SHOP-@WAIT-@WORK/TODO"
    nil))
 org-agenda-dim-blocked-tasks 'invisible
 org-agenda-files '("~/org")
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-deadline-prewarning-if-scheduled t
 org-agenda-skip-scheduled-if-done t
 org-agenda-todo-ignore-deadlines 'far
 org-agenda-todo-ignore-scheduled 'all
 org-archive-location "%s.bak::"
 org-capture-templates '(("t" "Todo" entry (file "~/org/inbox.org")
                          "* TODO %?" CAPTURED %u))
 org-completion-use-ido t
 org-default-notes-file "~/org/inbox.org"
 org-directory "~/org"
 org-enforce-todo-checkbox-dependencies t
 org-enforce-todo-dependencies t
 org-export-latex-tables-centered t
 org-hide-leading-stars t
 org-log-done 'time
 org-log-into-drawer t
 org-log-redeadline 'time
 org-log-refile 'time
 org-log-repeat 'time
 org-log-reschedule 'time
 org-log-states-order-reversed nil
 org-odd-levels-only t
 org-refile-allow-creating-parent-nodes 'confirm
 org-refile-use-outline-path 'file
 org-refile-targets '((nil :maxlevel . 3)
                      (org-agenda-files :maxlevel . 1))
 org-return-follows-link t
 org-stuck-projects '("+TODO=\"PROJ\"" ("TODO") nil "")
 org-tag-alist '(("@COMPUTER" . ?c)
                 ("@ERRAND"   . ?e)
                 ("@HOME"     . ?h)
                 ("@PHONE"    . ?p)
                 ("@READ"     . ?r)
                 ("@SHOP"     . ?s)
                 ("@WAIT"     . ?a)
                 ("@WORK"     . ?w))
 org-todo-keywords '((sequence "PROJ(p)" "TODO(t)"
                               "|"
                               "DONE(d)"))
 org-todo-repeat-to-state "TODO")

(defun org-agenda-toggle-blocked ()
  (interactive)
  (setq org-agenda-dim-blocked-tasks
        (if (eq org-agenda-dim-blocked-tasks 'invisible) t 'invisible)))

(defun el-get-org-mode-hook ()
  (local-set-key (kbd "C-c s") 'org-sort)
  (local-set-key (kbd "C-c p") 'org-promote-subtree)
  (local-set-key (kbd "C-c d") 'org-demote-subtree))

(global-set-key (kbd "C-x c") 'org-capture)
(global-set-key (kbd "C-x a") 'org-agenda)

;; DEFT (QUICK ORG FILES)

(setq deft-directory org-directory
      deft-extension "org"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t)

(global-set-key (kbd "C-x d") 'deft)
(global-set-key (kbd "C-x D") 'deft-new-file)

;; HASKELL / GHC-MOD / COMPANY-GHC

;; NOTE: GHC 7.10 w/ CABAL-1.22 OR GHC 7.8 w/ CABAL-1.20 (ONLY)
;;       https://github.com/kazu-yamamoto/ghc-mod/issues/417

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)

(defun my-haskell-mode-hook ()
  (local-set-key (kbd "C-c C-b") 'haskell-interactive-bring)
  (local-set-key (kbd "C-c C-c") 'haskell-process-cabal-build)
  (local-set-key (kbd "C-c C-i") 'haskell-process-do-info)
  (local-set-key (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (local-set-key (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (local-set-key (kbd "C-c C-s") 'haskell-interactive-switch)
  (local-set-key (kbd "C-c C-t") 'haskell-process-do-type)
  (local-set-key (kbd "C-c c")   'haskell-process-cabal)
  (local-set-key (kbd "C-c v c") 'haskell-cabal-visit-file)
  (local-set-key (kbd "SPC")     'haskell-mode-contextual-space))

(add-hook 'haskell-mode-hook 'ghc-init)
(add-hook 'haskell-mode-hook 'hindent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-ghc)
  (add-to-list 'company-backends 'company-cabal)
  (custom-set-variables '(company-ghc-show-info t)))

;; JAVASCRIPT

(setq auto-mode-alist (cons '("\\.js$" . js2-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.json$" . js2-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.julius$" . js2-mode) auto-mode-alist)
      js2-basic-offset 2)

;; CSS

(setq auto-mode-alist (cons '("\\.lucius$" . css-mode) auto-mode-alist))
