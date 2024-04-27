(use-package org
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (setq-default prettify-symbols-alist '(
                                         ("#+begin_src" . "✤")
                                         ("#+end_src" . "◆")
                                         (">=" . "≥")
                                         ("=>" . "⇨")))

  (setq default-buffer-file-coding-system 'utf-8
        ;; org-odd-levels-only t ;; odd levels only
        default-file-name-coding-system 'utf-8
        header-line-format nil ;; topmost margin => set to " "
        line-spacing 1
        locale-coding-system 'utf-8
        org-fontify-done-headline t ;; change the face of a headline if it is marked DONE
        org-hide-emphasis-markers t ;; hide markup (e.g. /.../ - italics; *...* - bold, etc.)
        org-hide-leading-stars nil ;; hide the stars
        org-image-actual-width '(300)
        org-pretty-entities t ;; show entities as UTF8 characters.
        org-src-tab-acts-natively t
        org-startup-indented nil
        org-startup-with-inline-images t
        org-todo-keyword-faces '(("TODO" . "yellow") ("PROGRESS" . "orange") ("DONE" . "green"))
        prettify-symbols-unprettify-at-point 'right-edge
        utf-translate-cjk-mode nil
        x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) (sql . t) (python . t) (emacs-lisp . t)))
  (setq org-confirm-babel-evaluate nil)

  :bind
  (:map org-mode-map
        ("C-," . org-insert-structure-template)
        ([remap org-shiftright] . org-demote-subtree)
        ([remap org-shiftleft] . org-promote-subtree)
        ("<left>" . outline-previous-visible-heading)
        ("<right>" . outline-next-visible-heading)
        ("S-<up>" . outline-up-heading))

  :hook
  ((org-mode . prettify-symbols-mode)))
   ;; (org-mode . org-auto-tangle-mode)
   ;; (org-mode . (lambda() (visual-line-mode t)))

;; (use-package org-superstar
;;   :hook (org-mode . org-superstar-mode))

(use-package org-bullets
  :config
  (setq org-bullets-face-name (quote org-bullet-face))
  (setq org-bullets-bullet-list
        ;; '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
        '("◉" "○" "✸" "✿" "✜" "◆" "▶"))
  :hook
  (org-mode . (lambda () (org-bullets-mode 1))))

;; (use-package olivetti
;;   :config
;;   (setq olivetti-body-width .9)
;;  :hook (org-mode . olivetti-mode))

(use-package org-transform-tree-table
  :after (org))

(provide 'init-org-mode)
