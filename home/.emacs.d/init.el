;;;; init --- d4ng's Emacs init.el
;;
;;;; Commentary:
;;
;; This is an init file. There's not much to see
;;

;;;; Code:


;;; Basics

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(unless (display-graphic-p) (menu-bar-mode -1))
(setq initial-frame-alist
      `((top . 0)
	(left . 0)
	(width . 100)
	(height . 1000))
      )

(fset 'yes-or-no-p 'y-or-n-p)
(setq user-full-name "Daniel Grady"
      user-mail-address "danielgrady@danielgrady.info"
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      custom-file (concat user-emacs-directory "custom.el")
      load-prefer-newer t
      require-final-newline t
      scroll-preserve-screen-position t
      sentence-end-double-space nil
      tab-always-indent 'complete
      visible-bell t)
(setq-default fill-column 80)

(defvar apropos-do-all t
  "Search more things with `apropos' commands.

If the variable `apropos-do-all' is non-nil, most apropos
commands behave as if they had been given a prefix argument.
There is one exception: `apropos-variable' without a prefix
argument will always search for all variables, no matter what the
value of `apropos-do-all' is.

https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html")

(if (file-readable-p custom-file)
    (load custom-file))

(add-hook 'text-mode-hook 'visual-line-mode)

(global-hl-line-mode t)
(savehist-mode 1)

;;; Configure packages

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package bind-key)
(use-package diminish)

(defun dang/macOS-p ()
  "Test whether Emacs is running on a Mac."
  (eq system-type 'darwin))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)))

(use-package adaptive-wrap
  :hook (text-mode . adaptive-wrap-prefix-mode))

(use-package anaconda-mode
  :diminish (anaconda-mode . " A")
  :hook ((python-mode . anaconda-mode)
	 (python-mode . anaconda-eldoc-mode)))

(use-package company
  :diminish " ɕ"
  :hook (prog-mode . company-mode))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package counsel
  :after (ivy smex)
  :bind (("M-x" . counsel-M-x)
	 ("C-c k" . counsel-ag)
	 ("<f1> b" . counsel-descbinds)
	 ("<f1> f" . counsel-describe-function)
	 ("<f1> v" . counsel-describe-variable)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c f" . counsel-git) ; Like `find-file'
	 ("C-c t" . counsel-git-grep)
	 ("C-s" . counsel-grep-or-swiper)
	 ("C-c u" . counsel-imenu)
	 ("<f1> S" . counsel-info-lookup-symbol)
	 ("C-c l" . counsel-locate)
	 ("C-c r" . counsel-rg)
	 ("C-c h" . counsel-shell-history)
	 ("M-`" . counsel-tmm)
	 ("<f2> u" . counsel-unicode-char)
	 ("M-y" . counsel-yank-pop))
  :custom
  (counsel-ag-base-command "ag --nocolor --nogroup --search-zip %s" "Search inside zip files with ag")
  )

(diminish 'eldoc-mode " λ?")

(electric-pair-mode t)

(use-package ensime
  :disabled)

(use-package exec-path-from-shell
  :if (and (dang/macOS-p) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize)
  (let ((gls (executable-find "gls")))
    (if gls (setq insert-directory-program gls))))

(use-package expand-region
  :bind
  ("s-'" . er/expand-region)
  ("C-c '" . er/expand-region))

(use-package flycheck
  :diminish " ✓"
  :init (global-flycheck-mode)
  :config
  ;; Pylint behaves poorly with the current release of Anaconda
  (setq-default flycheck-disabled-checkers '(python-pylint))
  ;; Set up mypy as a new syntax checker
  (flycheck-define-command-checker 'python-mypy
    "Use `mypy` to type-check Python code"
    :command '("mypy" "--show-column-numbers" source-original)
    :error-patterns
    '((error line-start (file-name) ":" line ":" column ": error:" (message) line-end))
    :modes 'python-mode
    )
  ;; Flycheck's checkers for Python don't chain together, so we need
  ;; to add this to the front of the list
  (add-to-list 'flycheck-checkers 'python-mypy)
  )

(use-package gist
  :bind (("C-c %" . gist-list)
	 ("C-c ^" . gist-region-or-buffer)))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package hydra)		   ; Make sure it’s installed (and also load it)
(require 'hydra)		   ; Silence warnings from the compiler

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package "isearch" ; giving name as string works around the lack of (provide ...)
  :ensure nil
  ;; swiper gets C-s
  :bind (("s-f" . isearch-forward-regexp)
	 ("C-r" . isearch-backward-regexp)))

(use-package ivy
  :demand t
  :diminish " ❦"
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-selectable-prompt t "Use C-p to turn the current text into a selectable item")
  (ivy-use-virtual-buffers t "Add `recentf-mode' and bookmarks to `ivy-switch-buffer'")
  (ivy-virtual-abbreviate 'full "Show full paths to virtual buffers")
  :bind ("C-c C-r" . ivy-resume))

(use-package ivy-hydra
  ;; Provides a useful hydra in ivy completion buffers; binds to C-o
  :after (ivy hydra))

;; JSON
(setq json-encoding-default-indentation "\t")

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.mdown\\'" . markdown-mode))
  :custom (markdown-command "multimarkdown"))

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-M->" . mc/unmark-next-like-this)
	 ("C-M-<" . mc/unmark-previous-like-this)
	 ("C-M-?" . mc/mark-all-dwim)
	 ("C-?" . mc/edit-lines)
	 ("s-<mouse-1>" . mc/add-cursor-on-click)))

;; The stock `org' is version 8 (I think). If you install a newer version of
;; `org' (currently at version 9), after the stock version has been loaded,
;; byte-compilation will do screwy things because of various major changes
;; (`org-babel-check-confirm-evaluate' changed from a macro to a function; other
;; stuff). This seems to be the simplest solution: make sure we install the
;; newest available version from the Org repositories, before `use-package'
;; starts pulling other things in. This will also mark `org-plus-contrib' as a
;; selected package, which is nice; this won’t happen otherwise because there’s
;; already a version of `org' installed so `(use-package org)' doesn’t mark it,
;; but `ox-reveal' depends on a newer version of `org', so `(use-package
;; ox-reveal)' ends up marking `org' as a dependency.
(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(use-package org
  :diminish (org-indent-mode " ⇥")
  :bind (("C-c c" . org-capture))
  ;; These are used in `dang/narrow-or-widen-dwim', and might (?) be called
  ;; before org is loaded. `org-indent-mode' should not need to be in this list,
  ;; but there’s some issue with `use-package' and byte compiler warnings.
  :commands (org-indent-mode org-edit-src-code org-narrow-to-block org-narrow-to-subtree)
  :custom
  (org-default-notes-file "~/org/¶ Notes.org")
  (org-log-done 'time)
  (org-use-speed-commands t)
  :config
  (defun dang/org-mode-hook ()
    "Set up line wrapping and indentation for `org-mode' just the way I like it."
    (adaptive-wrap-prefix-mode 0)
    (visual-line-mode 1)
    (org-indent-mode 1))
  (add-hook 'org-mode-hook 'dang/org-mode-hook)
  ;; Really?!? Really: https://orgmode.org/manual/Languages.html#Languages
  (add-to-list 'org-babel-load-languages '(shell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(use-package org-journal
  :bind ("C-c j" . org-journal-new-entry))

(use-package osx-clipboard
  ;; Terminal Emacs uses system clipboard on macOS. Does nothing if
  ;; enabled on other systems or in GUI Emacs.
  :config (osx-clipboard-mode 1)
  :diminish " ✄ ")

(use-package osx-trash
  :if (dang/macOS-p)
  :custom (delete-by-moving-to-trash t)
  :config (osx-trash-setup))

(use-package ox-reveal
  :after 'org
  :custom (org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/"))

(use-package paredit
  :diminish " ⁽₎"
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode lisp-mode lisp-interaction-mode scheme-mode) . enable-paredit-mode))

(use-package phi-search
  :bind (("M-C-s" . phi-search)
	 ("M-C-r" . phi-search-backward)))

(use-package projectile
  ;; There is also https://github.com/ericdanan/counsel-projectile, which adds
  ;; the ability to select from a list of actions / apply actions without
  ;; leaving the completion session.
  :custom (projectile-completion-system 'ivy)
  :bind ("C-c p" . projectile-mode))

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt")
  (python-fill-docstring-style 'django))

(use-package recentf
  :config (recentf-mode 1))

(use-package reveal-in-osx-finder
  :if (dang/macOS-p))

(use-package rust-mode
  :defer t)

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package racer
  :hook ((rust-mode . racer-mode)
	 (racer-mode . eldoc-mode)))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory "places"))
  (save-place-mode 1))

(use-package smart-mode-line
  :config (sml/setup))

(use-package smex)

(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :config (global-undo-tree-mode))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(diminish 'visual-line-mode " ↩")

(use-package wgrep
  :after (ivy counsel))

(use-package which-key
  :diminish " ‽"
  :config (which-key-mode))

(use-package winner
  :demand t
  :commands (winner-undo winner-redo)
  :config (winner-mode 1))


;;; Hydras

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-window (:hint nil)
   "
_h_ ←   _j_ ↓  _k_ ↑  _l_ →   _a_ce window
_q_ →←  _w_ ↕  _e_ ⇅  _r_ ←→  _b_alance

_v_ertical  horizon_t_al
_d_elete  _D_elete other  _o_nly  _O_nly other

_f_ind files  _S_ave buffer  _s_wap buffers

_F_ollow mode

_z_ undo  _Z_ redo

_SPC_ cancel"
   
   ;; Change window focus
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )

   ;; Change window size
   ("q" shrink-window-horizontally)
   ("w" enlarge-window)
   ("e" (lambda () (interactive) (enlarge-window -1)))
   ("r" enlarge-window-horizontally)
   ("b" balance-windows)

   ;; Create new windows
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
    )
   ("t" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
    )

   ;; Delete windows
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" delete-other-windows)
   ("O" ace-delete-other-windows)

   ;; Change buffers
   ("f" (call-interactively (global-key-binding (kbd "C-x C-f"))))
   ("S" save-buffer)
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))

   ;; Winner mode
   ("z" winner-undo)
   ("Z" winner-redo)

   ;; Other random stuff
   ("F" follow-mode)
   
   ("SPC" nil)
   )

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))


;;; Helper functions

;; Explain how to comment and uncomment lines to Emacs
(defun dang/comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
	(setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;; Explain how to un-wrap lines to Emacs
(defun dang/unfill-paragraph (&optional region)
  "Un-wrap the current paragraph or REGION into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

(defun dang/fill-or-unfill-paragraph ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column (if (eq last-command 'dang/fill-or-unfill-paragraph)
			 (point-max)
		       fill-column)))
    (call-interactively #'fill-paragraph)))

;; Explain how to scroll by single lines to Emacs
(defun dang/scroll-up-one-line ()
  "Scroll the view up one line."
  (interactive)
  (scroll-down 1))
(defun dang/scroll-down-one-line ()
  "Scroll the view down one line."
  (interactive)
  (scroll-up 1))

;; From http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun dang/narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))


;;; Non-package-related keybindings

(bind-key [remap fill-paragraph] 'dang/fill-or-unfill-paragraph)
(bind-key "M-Q" 'dang/unfill-paragraph)
(bind-key "s-/" 'dang/comment-or-uncomment-region-or-line)
(bind-key "C-c /" 'dang/comment-or-uncomment-region-or-line)
(bind-key "M-p" 'dang/scroll-up-one-line)
(bind-key "M-n" 'dang/scroll-down-one-line)
(bind-key "C-c n" 'dang/narrow-or-widen-dwim)
(when (dang/macOS-p)
  ;; Bug? Emacs doesn't see C-s-f
  (bind-key "<C-s-268632070>" 'toggle-frame-fullscreen)
  ;; Unbind the right ⌥ (Option) key for easier typing of spiffy characters.
  (setq mac-right-option-modifier nil)
  )
(bind-key "C-c o" 'hydra-window/body)
(bind-key "C-c m" 'hydra-multiple-cursors/body)

;; Local Variables:
;;   mode: emacs-lisp
;; End:

(provide 'init)
;;; init.el ends here
