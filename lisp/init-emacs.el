(use-package emacs
  :load-path "lisp"
  :after (dired-x)

  :config
  (global-hl-line-mode 1)
  (setenv "PAGER" "/bin/cat")
  (setenv "EDITOR" "/usr/bin/emacsclient")

  ;; (load-library "roy-minor-modes")

  (windmove-default-keybindings 'meta) ;; bind motions to M-[<arrow-keys>]

  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  :bind
  ("C-<f11>" . highlight-changes-mode)
  ("<f12>" . call-last-kbd-macro)
  ("<home>" . beginning-of-buffer)
  ("<end>" . end-of-buffer))

(setq apropos-do-all t
      mouse-yank-at-point t
      load-prefer-newer t
      frame-inhibit-implied-resize t
      custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))

;; TODO: move all this to better places.
(use-package emacs
  :config
  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (savehist-mode 1)
  (load custom-file))

(use-package
  emacs
  :if
  (display-graphic-p)
  :bind
  (("M-<up>" . #'scroll-other-window-down)
   ("M-<down>" . #'scroll-other-window))
  :hook (window-setup . (lambda ()
                          ;; this shows window frame..
                          (add-to-list 'initial-frame-alist '(fullscreen . fullboth)) ; was maximized
                          (add-to-list 'default-frame-alist '(fullscreen . fullboth))
                          (menu-bar-mode -1)
                          (tool-bar-mode -1)
                          (scroll-bar-mode -1)
                          (let ((fsize (cond ((eq system-type 'darwin) 154)
                                             ((eq system-type 'desktop) 124)
                                             ((eq system-type 'gnu/linux) 124)
                                             (t '112))))
                            (set-face-attribute 'default nil
                                                :family "Monaco"
                                                :height fsize)))))

(setq known-systems '(("dell" . "red")
                      ("ryzen" . "blue")
                      ("roy-m3" . "purple3")
                      ("t500" . "green")))
(defun _hostname()
  (car (split-string  (system-name) "\\.")))

(use-package emacs
  :if
  (assoc (_hostname) known-systems)
  :config
  (setq modus-vivendi-palette-overrides '((bg-main "#333333")
                                          (bg-mode-line-inactive bg-tab-other))
        modus-operandi-palette-overrides '((bg-main "#f8f8f8")))
  (menu-bar-mode -1) ;; disable menu bar (both gui and terminal modes)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (load-theme 'modus-vivendi t)
  (let ((color (cdr (assoc (_hostname) known-systems))))
    (custom-theme-set-faces
     'modus-vivendi
     '(ediff-fine-diff-A ((t (:background "light yellow" :foreground "black"))))
     '(ediff-fine-diff-B ((t (:background "light yellow" :foreground "black"))))
     `(spaceline-highlight-face ((t (:background ,color)))))))

(use-package emacs
  :if
  (string= (system-name) "devvm2907.frc0.facebook.com")
  :mode (("\\.buckconfig$" . shell-script-mode)
         ("\\.bcfg$"       . shell-script-mode))
  :config
  (setq url-proxy-services
    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
      ("http" . "fwdproxy:8080")
      ("https" . "fwdproxy:8080"))))

(use-package emacs
  :config
  (set-default 'truncate-lines t)
  (recentf-mode t)
  (setq tramp-allow-unsafe-temporary-files t)
  (setq completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t))


(use-package emacs
  :config
  (setq
    ;; '(auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
    auto-save-default t               ;; auto-save every buffer that visits a file
    auto-save-interval 200            ;; number of keystrokes between auto-saves (default: 300)
    auto-save-timeout 20              ;; number of seconds idle time before auto-save (default: 30)
    backup-by-copying t               ;; Copy all files, don't rename; don't clobber symlinks
    delete-by-moving-to-trash t
    delete-old-versions t             ;; delete excess backup files silently
    kept-new-versions 4               ;; Number of newest versions to keep.
    kept-old-versions 2               ;; oldest versions to keep when a new numbered backup is made (default: 2)
    make-backup-files t               ;; backup of a file the first time it is saved.
    version-control t                 ;; version numbers for backup files
    vc-make-backup-files t))

(use-package emacs
  :config
  (winner-mode 1)
  (global-set-key [f7] 'winner-undo)
  (global-set-key [f8] 'winner-redo))


(use-package xclip
  :if (display-graphic-p)
  :config
  (setq select-enable-clipboard t) ;; uses clipboard for cut/paste
  (setq select-enable-primary t) ;; uses selection for cut/paste
  (xclip-mode 1))


(provide 'init-emacs)
