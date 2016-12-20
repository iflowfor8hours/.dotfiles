;;;; init --- d4ng's Emacs init.el
;;
;;;; Commentary:
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
      apropos-do-all t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      custom-file (concat user-emacs-directory "custom.el")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(if (file-readable-p custom-file)
    (load custom-file))


;;; Load packages

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

;; TODO Projectile, Perspective, Purpose, Origami

(defun dang/system-is-mac () (eq system-type 'darwin))

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)))

(use-package adaptive-wrap
  :commands (adaptive-wrap-prefix-mode)
  :init (add-hook 'text-mode-hook 'adaptive-wrap-prefix-mode))

(use-package company
  :diminish " ɕ"
  :config
  (add-hook 'prog-mode-hook 'company-mode))

(use-package exec-path-from-shell
  :if (and (dang/system-is-mac) (display-graphic-p))
  :config
  (exec-path-from-shell-initialize))

(use-package expand-region
  :bind ("C-'" . er/expand-region))

(use-package hippie-exp
  :bind ("M-/" . hippie-expand))

(use-package hydra
  :commands defhydra)

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package "isearch" ; giving name as string works around the lack of (provide ...)
  :ensure nil
  ;; swiper gets C-s
  :bind (("s-f" . isearch-forward-regexp)
	 ("C-r" . isearch-backward-regexp)
	 ("M-C-s" . isearch-forward)
	 ("M-C-r" . isearch-backward)))

(use-package ivy
  ;; Force loading. Ivy automatically binds, for example, C-x b, but
  ;; won't do so until the package is loaded. We'd like these to be
  ;; available immediately.
  :demand t
  :diminish (ivy-mode . " ❦")
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’, and show
  ;; their full pathnames.
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)

  (use-package counsel
    :bind (("M-x" . counsel-M-x)
	   ("C-x C-f" . counsel-find-file)
	   ("<f1> b" . counsel-descbinds)
	   ("<f1> f" . counsel-describe-function)
	   ("<f1> v" . counsel-describe-variable)
	   ("<f1> l" . counsel-load-library)
	   ("<f2> i" . counsel-info-lookup-symbol)
	   ("<f2> u" . counsel-unicode-char)
	   ("C-c k" . counsel-ag)))

  (use-package swiper
    :bind ("C-s" . swiper)))

(use-package magit
  :bind ("C-c g" . magit-status))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode)
	 ("\\.mdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

(use-package misc
  :ensure nil
  :bind ("M-z" . zap-up-to-char))

;; Maybe also look in to phi-search?
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-M->" . mc/unmark-next-like-this)
	 ("C-M-<" . mc/unmark-previous-like-this)
	 ("C-M-?" . mc/mark-all-dwim)
	 ("C-?" . mc/edit-lines)
	 ("s-<mouse-1>" . mc/add-cursor-on-click)))

(use-package org
  :bind (("C-c c" . org-capture))
  :config
  (setq org-default-notes-files "~/org/¶ Notes.org")
  (add-hook 'org-mode-hook 'dang/org-mode-hook))

(use-package org-journal
  ;; This is the default binding, but providing it will cause lazy
  ;; loading.
  :bind ("C-c C-j" . org-journal-new-entry))

(use-package osx-clipboard
  ;; Terminal Emacs uses system clipboard on macOS. Does nothing if
  ;; enabled on other systems or in GUI Emacs.
  :config (osx-clipboard-mode 1))

(use-package osx-trash
  :if (dang/system-is-mac)
  :config
  (osx-trash-setup)
  (setq delete-by-moving-to-trash t))

(use-package paredit
  :commands (paredit-mode enable-paredit-mode)
  :diminish (paredit-mode . " ⁽₎")
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'scheme-mode-hook 'enable-paredit-mode)
  )

;; (use-package parinfer
;;   :commands parinfer-mode
;;   :bind ("C-," . parinfer-toggle-mode)
;;   :init (add-hook 'emacs-lisp-mode-hook #'parinfer-mode))

(use-package recentf
  :init (recentf-mode 1))

(use-package reveal-in-osx-finder
  :if (dang/system-is-mac))

(use-package saveplace
  :config
  (setq save-place-file (concat user-emacs-directory "places"))
  (setq-default save-place t))

(use-package smart-mode-line
  :commands (sml/setup))

(use-package solarized-theme
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark)
  (sml/setup))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

(use-package which-key
  :diminish " ‽"
  :config (which-key-mode))

(use-package winner
  :demand t
  :config (winner-mode 1))


;;; Helper functions

(defun dang/org-mode-hook ()
  "Set up line wrapping and indentation for org-mode just the way I like it."
  (adaptive-wrap-prefix-mode 0)
  (visual-line-mode 1)
  (org-indent-mode 1))

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
  "Takes a multi-line paragraph and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;; Explain how to scroll by single lines to Emacs
(defun dang/scroll-up-one-line ()
  (interactive)
  (scroll-down 1))
(defun dang/scroll-down-one-line ()
  (interactive)
  (scroll-up 1))


;;; Non-package-related keybindings

(global-set-key (kbd "M-Q") 'dang/unfill-paragraph)
(global-set-key (kbd "s-/") 'dang/comment-or-uncomment-region-or-line)
(global-set-key (kbd "M-p") 'dang/scroll-up-one-line)
(global-set-key (kbd "M-n") 'dang/scroll-down-one-line)
(when (dang/system-is-mac)
  ;; Bug? Emacs doesn't see C-s-f
  (global-set-key (kbd "<C-s-268632070>") 'toggle-frame-fullscreen))


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
   ("O" ace-maximize-window)

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

(global-set-key (kbd "C-c o") 'hydra-window/body)

(defhydra hydra-outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" hide-sublevels)    ; Hide everything but the top-level headings
  ("t" hide-body)         ; Hide everything but headings (all body lines)
  ("o" hide-other)        ; Hide other branches
  ("c" hide-entry)        ; Hide this entry's body
  ("l" hide-leaves)       ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree)      ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)     ; Show this heading's immediate child sub-headings
  ("k" show-branches)     ; Show all sub-headings under this heading
  ("s" show-subtree)      ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)                ; Up
  ("n" outline-next-visible-heading)      ; Next
  ("p" outline-previous-visible-heading)  ; Previous
  ("f" outline-forward-same-level)        ; Forward - same level
  ("b" outline-backward-same-level)       ; Backward - same level
  ("z" nil "leave"))

(global-set-key (kbd "C-c #") 'hydra-outline/body) ; by example

;; Local Variables:
;;   mode: emacs-lisp
;; End:
