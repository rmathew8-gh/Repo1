;; -------- preamble -----------------------------------------------------
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))

(require 'use-package)

(use-package 
  package 
  :custom (use-package-always-ensure t) 
  (use-package-expand-minimally t) 
  :init (add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp")))

;; --------- use-package local stuff --------------------------------------

;; [sara@t500 Ex.Roy]$ pip list
;; pyflakes           3.0.1
;; pylint             2.17.4
;; pyright            1.1.313
;; yapf               0.40.0
;; pytest             7.3.2
;; pytest-pythonpath  0.7.4

(use-package 
  which-key 
  :init (which-key-mode))

(use-package 
  vertico 
  :config (vertico-mode t))

(use-package 
  orderless 
  :custom (completion-styles '(orderless)))

(use-package 
  magit)

(use-package 
  pyvenv 
  :init (setenv "WORKON_HOME" "~/.virtualenvs") 
  (pyvenv-workon "py"))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package 
  savehist 
  :init (savehist-mode t))

(use-package 
  pytest 
  :init (setq pytest-cmd-flags "-x -s"))

;; python-pytest is needed to overcome pytest test-directory discovery quirks
(use-package 
  python-pytest)

(use-package 
  yasnippet 
  :ensure t)

(use-package 
  flycheck 
  :ensure t 
  :init (global-flycheck-mode))

;; yapf is the formatting package for python code.
(use-package 
  py-yapf)

(use-package 
  python 
  :after (py-yapf) 
  :bind (:map inferior-python-mode-map
	      ("C-c C-k" . (lambda() 
			     (interactive) 
			     (comint-clear-buffer)))) 
  :hook ((comint-output-filter-functions . python-pdbtrack-comint-output-filter-function) 
	 (python-mode . py-yapf-enable-on-save)))

;; lsp mode has all the goodies.
(use-package 
  lsp-mode 
  :init (setq lsp-headerline-breadcrumb-enable nil ;; avoids "Error running timer 'lsp--on-idle' issues"
	      lsp-enable-file-watchers nil lsp-file-watch-threshold 100) 
  :hook (lsp-mode . lsp-enable-which-key-integration) 
  :commands (lsp lsp-deferred))

;; format python files on save; must do `pip install yapf`
(use-package 
  lsp-pyright 
  :hook ((python-mode . (lambda () 
			  (require 'lsp-pyright) 
			  (lsp) 
			  (add-hook 'before-save-hook 'lsp-format-buffer nil t)))))

;; formats elisp files on save.
(use-package 
  elisp-format 
  :hook (emacs-lisp-mode . (lambda () 
			     (add-hook 'before-save-hook 'elisp-format-buffer  nil t))))

(use-package 
  emacs 
  :init (setq modus-vivendi-palette-overrides '((bg-main "#333333") 
						(bg-mode-line-inactive bg-tab-other))
	      modus-operandi-palette-overrides '((bg-main "#f8f8f8"))) 
  (menu-bar-mode -1) ;; disable menu bar (both gui and terminal modes)
  (setq modus-themes-bold-constructs t) 
  (setq modus-themes-italic-constructs t) 
  (load-theme 'modus-vivendi t))

;; --------- customization ------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(elisp-format py-yapf flycheck yasnippet lsp-pyright lsp-mode
					    python-pytest pytest pyvenv magit orderless vertico
					    which-key use-package)) 
 '(split-width-threshold 100))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
