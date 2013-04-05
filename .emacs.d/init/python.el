(add-hook
 'python-mode-hook
 (lambda ()
   (setq indent-tabs-mode t
         python-guess-indent nil
         python-indent 4
         tab-width 2)))
