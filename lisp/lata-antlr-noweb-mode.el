;;; package --- Summary.
;;; Commentary:
;;; Code:

(defconst chunk-decl-re "^<<\\(.*?\\)>>=$")
(defconst chunk-use-re "<<\\(.*?\\)>>")

(defvar lata-antlr-noweb-mode-hook nil
  "list of functions to call when entering lata-antlr-noweb-mode")


(defvar lata-antlr-noweb-mode-map nil  ; Create a mode-specific keymap.
  "keymap for lata-antlr-noweb-mode major mode")

(if lata-antlr-noweb-mode-map
    () ; Do not change the keymap if already set up.
  (setq lata-antlr-noweb-mode-map (make-sparse-keymap))

  (define-key lata-antlr-noweb-mode-map "@" 'lata-antlr-noweb-electric-@)
  (define-key lata-antlr-noweb-mode-map "<" 'lata-antlr-noweb-electric-<)
  (define-key lata-antlr-noweb-mode-map (kbd "C-=") 'lata-antlr-noweb-electric-=)

  (define-key lata-antlr-noweb-mode-map [f4] 'lata-antlr-tangle-all-subtree)
  (define-key lata-antlr-noweb-mode-map [S-f4] 'lata-antlr-weave-subtree)
  (define-key lata-antlr-noweb-mode-map [C-f4] 'lata-antlr-weave-except-subtree)

  (define-key lata-antlr-noweb-mode-map [f5] 'lata-antlr-tangle-all)
  (define-key lata-antlr-noweb-mode-map [S-f5] 'lata-antlr-force-tangle-all)

  (define-key lata-antlr-noweb-mode-map [left] 'lata-antlr-goto-prev-chunk-use)
  (define-key lata-antlr-noweb-mode-map [right] 'lata-antlr-goto-next-chunk-use)
  (define-key lata-antlr-noweb-mode-map [(shift left)] 'lata-antlr-goto-prev-chunk-decl)
  (define-key lata-antlr-noweb-mode-map [(shift right)] 'lata-antlr-goto-next-chunk-decl))


; function to invoke this major mode
(defun lata-antlr-noweb-mode ()
  "Major mode for editing lata-antlr-noweb intended for humans to read.
  Special commands: \\{lata-antlr-noweb-mode-map}

  Turning on lata-antlr-noweb-mode runs the hook `lata-antlr-noweb-mode-hook'."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'lata-antlr-noweb-mode)     ; `describe-mode' uses this string
  (setq mode-name "Lata-Antlr-Noweb")      ; This name goes into the mode line.

  (setq local-abbrev-table lata-antlr-noweb-mode-abbrev-table)
  (set-syntax-table lata-antlr-noweb-mode-syntax-table)
  (use-local-map lata-antlr-noweb-mode-map)

  (make-local-variable 'paragraph-start)
  (make-local-variable 'paragraph-separate)
  (setq paragraph-start    "^<<.*>>=\\|^@\\s-\\|^@$")
  (setq paragraph-separate "^<<.*>>=\\s-*$\\|^@\\s-\\|^@$")
  
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lata-antlr-noweb-font-lock-keywords t))

  (run-hooks 'lata-antlr-noweb-mode-hook))     ; Finally, this permits the user to
;   customize the mode with a hook.


; Use this func to determine the region for the comment chunk pattern, since
; we can't use an emacs regexp (they are greedy and don't work right).
(defun roy-match-comment-chunk(bound)
  "my fun"
  (let*
      ((p1 (and (re-search-forward "^@$\\|^@\\s-" bound t)  (match-beginning 0)))
       (p2 (and (re-search-forward "^<<" bound t) (match-beginning 0))))

    (if (not (and p1 p2))
    nil             ; return nil if RE not found
      (set-match-data (list p1 p2))
      t)))              ; return t to indicate success (match is set)


(defun la--next-chunkname ()
  "Find chunk forward."
  (save-excursion
    (beginning-of-line)
    (or (and (looking-at chunk-decl-re) (match-string-no-properties 1))
        (and (re-search-forward chunk-decl-re) (match-string-no-properties 1)))))

(defun la--prev-chunkname ()
  "Find chunk backward."
  (save-excursion
    (or (and (looking-at chunk-decl-re) (match-string-no-properties 1))
        (and (re-search-backward chunk-decl-re) (match-string-no-properties 1)))))


(defun la-chunk-use-positions (cur line-start line-text)
  (interactive)

  (cl-defun
   in--range (n (a b)) ;; notice destructuring
   (and (>= n a) (< n b)))

  (let ((-start 0)
        (-positions '()))
    (while (string-match "<<\\(.*?\\)>>" line-text -start)
      (let ((p0 (+ line-start (match-beginning 0)))
            ;; (s (match-string-no-properties 1 line-text))
            (p1 (+ line-start (match-end 0))))
        ;; (message "%d %d [%s]" p0 p1 s)
        (setq -positions (append -positions (list (list p0 p1))))
        (setq -start (match-end 0))))
    (cl-remove-if-not (lambda (pos) (in--range cur pos)) -positions)))


(defun lata-antlr-goto-next-chunk-decl ()
  "Go to next chunk."
  (interactive)
  (if (looking-at chunk-decl-re) (forward-char 1))
  (re-search-forward chunk-decl-re)
  (beginning-of-line)
  (match-string-no-properties 1))

(defun lata-antlr-goto-prev-chunk-decl ()
  "Go to next chunk."
  (interactive)
  (re-search-backward chunk-decl-re)
  (and (looking-at chunk-decl-re) (match-string-no-properties 1)))

(defun lata-antlr-goto-next-chunk-use ()
  "Go to next chunk."
  (interactive)

  ;; if already on chunk, the nav to next
  (if (looking-at chunk-use-re)
      (let ((-chunk-name (match-string-no-properties 1))
            (-chunk-pattern (match-string-no-properties 0)))
        (if (search-forward -chunk-pattern nil t 2)
            (goto-char (match-beginning 0)))
        -chunk-name) ;; return chunkname

    ;; else, find next candidate
    (let* ((a (line-beginning-position))
           (b (line-end-position))
           (s (buffer-substring a b))
           (pair (car (la-chunk-use-positions (point) a s))))

      (if pair
          (let ((s (buffer-substring (point) (nth 1 pair))))

            (if (not (search-forward s nil t 2))
                nil
              ;; found a partial match
              (goto-char (match-beginning 0))
              s))
        (if (not (re-search-forward chunk-use-re nil t))
            nil
          (goto-char (match-beginning 0)) ;; leave point at beginning
          (match-string-no-properties 1))))))

(defun lata-antlr-goto-prev-chunk-use ()
  "Go to prev chunk."
  (interactive)

  ;; if already on chunk, the nav to prev
  (if (looking-at chunk-use-re)
      (let ((-chunk-name (match-string-no-properties 1))
            (-chunk-pattern (match-string-no-properties 0)))
        (if (search-backward -chunk-pattern nil t)
        -chunk-name)) ;; return chunkname

    ;; else, find prev candidate
    (let* ((a (line-beginning-position))
           (b (line-end-position))
           (s (buffer-substring a b))
           (pair (car (la-chunk-use-positions (point) a s))))

      (if pair
          (let ((s (buffer-substring (point) (nth 1 pair))))

            (if (not (search-backward s nil t))
                nil
              ;; found a partial match
              s))
        (if (not (re-search-backward chunk-use-re nil t))
            nil
          (match-string-no-properties 1))))))


(defvar lata-antlr-noweb-mode-syntax-table nil
  "Syntax table used while in lata-antlr-noweb mode.")

(if lata-antlr-noweb-mode-syntax-table
    ()              ; Do not change the table if it is already set up.
    (setq lata-antlr-noweb-mode-syntax-table (make-syntax-table))
    (modify-syntax-entry ?\" ".   " lata-antlr-noweb-mode-syntax-table)
    ;(modify-syntax-entry ?\' ".   " lata-antlr-noweb-mode-syntax-table)
    (modify-syntax-entry ?\\ ".   " lata-antlr-noweb-mode-syntax-table))

(defvar lata-antlr-noweb-mode-abbrev-table nil
  "Abbrev table used while in lata-antlr-noweb mode.")
(define-abbrev-table 'lata-antlr-noweb-mode-abbrev-table ())

(defvar lata-antlr-noweb-font-lock-keywords 
  '((roy-match-comment-chunk  .   'font-lock-comment-face)   ;; lata-antlr-noweb-comment-face
    ("^\\(<<.*>>=\\)\W*$"     1   'bookmark-face)            ;; 'lata-antlr-noweb-code-decl-face
    ("\\(<<.*>>\\)[^=]"       1   'font-lock-constant-face)) ;; lata-antlr-noweb-code-instance-face)))
  ;; '((roy-match-comment-chunk  .   'lata-antlr-noweb-comment-face)
  ;;   ("^\\(<<.*>>=\\)\W*$"     1   'lata-antlr-noweb-code-decl-face)
  ;;   ("\\(<<.*>>\\)[^=]"       1   'lata-antlr-noweb-code-instance-face))
  "Additional expressions to highlight in Shell mode.")



(defun lata-antlr-rearrange-all
    (&optional
     action)
  "Call nw funcs.
ACTION is one of ..."
  (interactive "P")
  (let* ((choices '(("rearrange")
                    ("drop")
                    ("keep")))
         (_action (cond ((eq action nil) "rearrange")
                        (t (completing-read "action [rearrange]: " choices nil t nil nil
                                            "rearrange"))))
         (out-buf (not (equal _action "keep"))))
    (save-excursion (shell-command-on-region (point-min)
                                             (point-max) "python -m antlr_noweb.tangle" out-buf out-buf nil
                                             t))))


(defun lata-antlr-tangle-all ()
  "Generate all files (genFiles)."
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max) "python -m antlr_noweb.tangle -a tangle"))

(defun lata-antlr-force-tangle-all ()
  "Generate all files, do mkdirs as needed (genFiles)."
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max) "python -m antlr_noweb.tangle --mkdir -a tangle"))


(defun lata-antlr-tangle-all-subtree ()
  "Expand a subtree (tangle)."
  (interactive)
  (let ((name (la--prev-chunkname)))
    (save-excursion (shell-command-on-region (point-min)
                                             (point-max)
                                             (concat "python -m antlr_noweb.tangle -a tangle -R \""
                                                     name "\"")))))

(defun lata-antlr-weave-subtree ()
  "Copy an entire subtree."
  (interactive)
  (let ((name (la--prev-chunkname)))
    (save-excursion (shell-command-on-region (point-min)
                                             (point-max)
                                             (concat "python -m antlr_noweb.tangle -a rearrange -R \""
                                                     name "\"")))))

(defun lata-antlr-weave-except-subtree ()
  "Copy all except subtree."
  (interactive)
  (let ((name (la--prev-chunkname)))
    (save-excursion (shell-command-on-region (point-min)
                                             (point-max)
                                             (concat "python -m antlr_noweb.tangle --action remove -R \""
                                                     name "\"")))))



;; modification
(defun lata-antlr-noweb-electric-= ()
  "copy chunk name into kill ring"
  (interactive)

  (let ((pm (point-marker)) ; preserve position
        (name)) ; variable in scope
    (lata-antlr-next-chunk-defn t) ; find chunk we're in
    (setq name (match-string-no-properties 1)) ; chunk name to clone

    (lata-antlr-next-chunk-defn t) ; find previous chunk
    (lata-antlr-next-chunk-fn t) ; move to previous instance

    (let* ((p1 (point))
           (_ (beginning-of-line 1))
           (len (- p1 (point)))
           (blanks (make-string len #x20))
           (cstr (format "%s<<%s>>" blanks name)))
      (beginning-of-line 1)
      (forward-line)
      (open-line 1)
      (insert cstr)
      (message (format "inserted: <<%s>> above" name))
      (goto-char pm))))

(defun lata-antlr-noweb-electric-@ ()
  "Intelligently insert @ with a space"
  (interactive)
  (if (not (bolp))
      (self-insert-command 1)
    (beginning-of-line)
    (if (eolp) nil (kill-line))
    (insert (format-time-string "@ updated: %b/%d/%y"))))

(defun lata-antlr-noweb-electric-< ()
  "Begin chunk definition (if within doc chunk)"
  (interactive)
  (if (not (bolp))
      (self-insert-command 1)
      (insert (format-time-string "\n@ updated: %b/%d/%y\n"))
      (insert "<<")
    (save-excursion
      (insert ">>")
      (if (not (looking-at "\\s *$"))
          (newline)))))

;; <:lata-antlr-noweb-mode:defun: electric = char>

(provide 'lata-antlr-noweb-mode)
;;; lata-antlr-noweb-mode.el ends here
