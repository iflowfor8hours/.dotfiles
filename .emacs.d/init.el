;;; init --- d4ng's Emacs init.el
;;;
;;; Commentary:
;;; This is an init file. There's not much to see
;;;
;;; Code:

;; We need … moar … windows!!!
(add-hook 'window-setup-hook 'toggle-frame-maximized t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(
				  better-defaults
				  exec-path-from-shell async smex ido-ubiquitous paradox neotree
				  smooth-scrolling multiple-cursors paredit expand-region flycheck guru-mode
				  org
				  magit
				  elpy ;; jedi anaconda-mode virtualenvwrapper
				  markdown-mode
				  auctex auctex-latexmk
				  )
  "Install these at launch.  If they aren't already.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'better-defaults)

;; better-defaults turns off the menu bar. The menu bar is good.
(menu-bar-mode nil)

;; For the exec-path-from-shell package
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Make mouse scrolling work better
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; (setq mouse-wheel-progressive-speed nil)

;; The Anaconda binaries are not on PATH by default, because Homebrew
;; complains about them. (Needs to come after exec-path-from-shell.)
(let ((anaconda-path (concat (getenv "HOME") "/anaconda/bin")))
  (setenv "PATH" (concat anaconda-path ":" (getenv "PATH")))
  (setq exec-path (append (list anaconda-path) exec-path))
  )

;; IDO & Smex
(require 'ido-ubiquitous)
(ido-mode 1)
(ido-everywhere 1)
(ido-ubiquitous-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ace Jump
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; What should we do about white space? Spaces should be left blank;
;; tabs should be visually distinct from spaces.
(require 'whitespace)
(global-whitespace-mode)

;; Python
;; (require 'python)
;; (setq python-shell-interpreter "ipython")
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)

;; Turn on FlyCheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Set up org capture
(require 'org)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map (kbd "C-c c") 'org-capture)

;; Set file extensions for markdown-mode
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Use paredit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
(add-hook 'scheme-mode-hook                      #'enable-paredit-mode)

;; Better table editing in Markdown
(add-hook 'markdown-mode-hook 'turn-on-orgtbl)

;; Bind commands for multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Bind commands for expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Set up AUCTeX
(require 'reftex) ;; RefTeX is now built in to Emacs
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook (lambda ()
					    (progn
						 (setq indent-tabs-mode t)
						 (setq indent-line-function 'indent-relative))))
(auctex-latexmk-setup)

;; Explain how to comment and uncomment lines to Emacs
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
        (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))
(global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)

;; Explain how to un-wrap lines to Emacs
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))
(define-key global-map "\M-Q" 'unfill-paragraph)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-auto-save t)
 '(TeX-command-list
   (quote
    (("latexmk-live" "latexmk -pdf -pvc %t" TeX-run-command nil
	 (plain-tex-mode latex-mode doctex-mode)
	 :help "Start a latexmk process to watch for changes")
	("LatexMk" "latexmk %(-PDF)%S%(mode) %t" TeX-run-latexmk nil
	 (plain-tex-mode latex-mode doctex-mode)
	 :help "Run LatexMk")
	("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
	 (plain-tex-mode texinfo-mode ams-tex-mode)
	 :help "Run plain TeX")
	("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
	 (latex-mode doctex-mode)
	 :help "Run LaTeX")
	("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
	 (texinfo-mode)
	 :help "Run Makeinfo with Info output")
	("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
	 (texinfo-mode)
	 :help "Run Makeinfo with HTML output")
	("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
	 (ams-tex-mode)
	 :help "Run AMSTeX")
	("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
	 (context-mode)
	 :help "Run ConTeXt once")
	("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
	 (context-mode)
	 :help "Run ConTeXt until completion")
	("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
	("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
	("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
	("Print" "%p" TeX-run-command t t :help "Print the file")
	("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
	("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
	("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
	("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
	("Check" "lacheck %s" TeX-run-compile nil
	 (latex-mode)
	 :help "Check LaTeX file for correctness")
	("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
	 (latex-mode)
	 :help "Check LaTeX file for common mistakes")
	("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
	("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
	("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
	("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-parse-self t)
 '(TeX-source-correlate-mode t)
 '(auctex-latexmk-inherit-TeX-PDF-mode t)
 '(cua-mode t nil (cua-base))
 '(deft-directory "/Users/dgrady/Dropbox/Notes")
 '(ein:use-auto-complete t)
 '(indent-tabs-mode t)
 '(message-user-organization "Aleph Naught Enterprises")
 '(org-directory "~/Dropbox/org")
 '(org-startup-indented t)
 '(org-startup-truncated nil)
 '(paradox-automatically-star t)
 '(paradox-github-token "6f3f08d7319356ea4f187a903ddc508cc8d3bcc3")
 '(reftex-cite-format "\\autocite{%l}")
 '(reftex-plug-into-AUCTeX t)
 '(scroll-error-top-bottom t)
 '(sentence-end-double-space nil)
 '(tab-width 5)
 '(text-mode-hook (quote (text-mode-hook-identify turn-on-visual-line-mode)))
 '(whitespace-style (quote (tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
