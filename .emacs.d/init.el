;; Stash those stupid custom settings elsewhere

(setq custom-file "~/.emacs.d/custom.el")
(if (file-readable-p custom-file)
    (load custom-file))

;; Stash those stupid backups elsewhere

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Who am I?

(setq user-full-name "Daniel Grady"
      user-mail-address "daniel.grady@idanalytics.com")

;; Bootstrap use-package

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

;; Helper functions

(defun dang/org-mode-hook ()
  "Set up line wrapping and indentation for org-mode just the way I like it."
  (adaptive-wrap-prefix-mode 0)
  (visual-line-mode 1)
  (org-indent-mode 1))

;; Load packages

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.mdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

(use-package org
  :bind (("C-c c" . org-capture))
  :config
  (setq org-default-notes-files "~/org/¶ Notes.org")
  (add-hook 'org-mode-hook 'dang/org-mode-hook))

(use-package org-journal
  ; Provides good defaults, no config necessary
  )

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-M->" . mc/unmark-next-like-this)
	 ("C-M-<" . mc/unmark-previous-like-this)
	 ("C-M-?" . mc/mark-all-dwim)
	 ("s-<mouse-1>" . mc/add-cursor-on-click)))

(use-package expand-region
  :bind ("C-'" . er/expand-region))

(use-package adaptive-wrap
  :commands (adaptive-wrap-prefix-mode)
  :init (add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode))

(use-package which-key
  :diminish "‽"
  :config (which-key-mode))

(use-package magit
  :bind ("C-c g" . magit-status))

;; Local Variables:
;;   mode: emacs-lisp
;; End:
