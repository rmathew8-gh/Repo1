(defalias 'g  'goto-line)
(defalias 'dt 'delete-trailing-whitespace)
(defalias 'o  'occur)
(defalias 'eb 'ediff-buffers)
(defalias 'er 'erase-buffer)
(defalias 'rr 'replace-regexp)
(defalias 'rs 'replace-string)
(defalias 'sl 'sort-lines)
(defalias 'tt 'toggle-truncate-lines)

(defalias 'ms 'magit-status)
(defalias 'sbke  'save-buffers-kill-emacs)


(defun gf(&optional arg)
  "search; w/prefix, does keas search only; always prunes .git"
  (interactive "p")

   (let*
       ((template "find %s %s -prune -o %s -type f -print0 | xargs -0 grep -in -e " )
    (prunes "\\( -name \".git\" -o -name \"ext-2.2\" -o -name \"Twisted*\" -o -name \"slave*\" -o -name \"external*\" \\)")
    (specifics "\\( -name \"*.py\" -o -name \"*.html\" -o -name \"*.js\" -o -name \"*.txt\" -o -name \"*.pt\" -o -name \"*.zcml\" \\) ")
    (big (format template "*" prunes specifics ))
    (small (format template "."  "-name \".git\" " "" ""))
    (command (cond ((eq 1 arg) small) (t big)))
    (command (cons command (+ 1 (length command))))
    (command (read-from-minibuffer "Run find (like this): " command nil nil 'grep-find-history)))
     (grep-find command)))

;; -I: ignore binaries
;; \":(exclude)localization\": ignore dirs (recursive)
(defun gg(arg1)
  "git-grep on code"
  (interactive "spattern: ")
  (grep (concat "git grep -iIn -e \"" arg1 "\" \":(exclude)localization\" .")))

(defun ie-get-bare-name (filename) 
  (let ((file-re "\\.?\\(.*\\).\\(org\\|nw\\)$")) 
    (and (string-match file-re filename) 
         (match-string-no-properties 1 filename))))

(defun ie() 
  "Does an info-edit on the argument" 
  (interactive)
  (setq roy-info-keys
        (let* ((dir-1 (expand-file-name "~/git-dir/Noweb-Files/Literate-Programming")) 
               (dir-2 (expand-file-name "~/git-dir/Noweb-Files.Personal/Literate-Programming")) 
               (pattern "\\(org\\|nw\\)$") 
               (files-1 (directory-files dir-1 nil pattern)) 
               (files-2 (directory-files dir-2 nil pattern)) 
               (map-fn   (apply-partially (lambda(d f) 
                                            (cons (ie-get-bare-name f) 
                                                  (concat d "/" f))))) 
               (map-fn-1 (apply-partially map-fn dir-1)) 
               (map-fn-2 (apply-partially map-fn dir-2)))
          (nconc (mapcar map-fn-1 files-1) 
                 (mapcar map-fn-2 files-2))))
  (let* ((roy-ie-key (completing-read "Enter info-edit key: " roy-info-keys nil t nil))) 
    (find-file (cdr (assoc roy-ie-key roy-info-keys)))))

(defun rb()
  (interactive)
  (let ((new-name (read-string "rename-to: " (buffer-name))))
    (rename-buffer new-name)))

 (defun roy-erase-comint-buffer()
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

(defun roy-org-export-md()
  (interactive)
  (let ((buf (org-md-export-as-markdown)))
    (set-buffer buf)
    (flush-lines "^$")
    (beginning-of-buffer)
    (kill-line 1)
    (beginning-of-buffer)
    (replace-regexp "^#" "\n#")))

(defun roy-show-multiple-windows(elems)

  (delete-other-windows)
  (switch-to-buffer (elt elems 0))

  (split-window-right)
  (other-window 1)
  (switch-to-buffer (elt elems 1))

  (cond ((> (length elems) 2)
         (progn
           (split-window-below)
           (other-window 1)
           (switch-to-buffer (elt elems 2)))))

  (cond ((> (length elems) 3)
         (progn
           (other-window 1)
           (split-window nil 10 'above)
           (switch-to-buffer (elt elems 3))
           (other-window 2))))

  (other-window 1))

(defun roy-table-to-csv()
  (interactive)
  (save-excursion
    (set-mark (re-search-forward "</table>" nil t))
    (re-search-backward "<table>" nil t)
    (shell-command-on-region (region-beginning) (region-end) "table-to-csv.sh" :replace t)))

(defun roy/create-org-link(beg end)
  "select a region; convert it to an org-link using url on next line"
  (interactive "r")
  (let ((desc)(link))
    (save-excursion
      (next-line)
      (org-beginning-of-line)
      (if (looking-at ".*\\(https://.*$\\)")
          (progn (setq desc (buffer-substring-no-properties beg end)
                       link (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                 (beginning-of-line)
                 (kill-line 1))))
    (org-insert-link nil link desc)))

(defun roy/org-strip-properties(&optional beg end)
  (interactive "r")
  (let ((beg  (or beg (point-min)))
        (end  (or end (point-max))))
    (shell-command-on-region beg end "org-strip-properties.sh" :replace t)))

(defun slu(args)
  "sort and make lines unique in a region; if no mark, operates on whole region"
  (interactive "sargs to uniq (eg: -c): \n")
  (let
      ((beg (if mark-active (region-beginning) 1))
       (end (if mark-active (region-end) (buffer-size))))
    ;;(message "beg: %s; end: %s; args: %s" beg end args)
    (sort-lines nil beg end)
    (shell-command-on-region beg end (concat "uniq " args) nil t)))

(defun roy-get-ediff-region (buf-type)
  "invoke as (insert (roy-get-ediff-region 'A))"
  (with-current-buffer "*Ediff Control Panel*"
    (let*
        ((n ediff-current-difference)
         (start (ediff-get-diff-posn buf-type 'beg n (current-buffer)))
         (end   (ediff-get-diff-posn buf-type 'end n (current-buffer)))
         (buf (or (and (eq buf-type 'A) ediff-buffer-A) ediff-buffer-B)))
      (with-current-buffer buf
        (buffer-substring-no-properties start end)))))

(defun roy-grep-for-info(ss kw &optional pattern)
  "search thru .nw files (kustomer, fb or personal)"
  (let*
      ((dirs '((:kustomer . "~/git-dir/Kustomer/Literate-Programming")
              (:facebook . "~/git-dir/Facebook/Literate-Programming")
              (:kust-code . "~/git-dir/Kustomer/kustomer")
              (:all . "~/git-dir")
              (:personal . "~/git-dir/Noweb-Files")))
       (dir (cdr (assoc kw dirs)))
       (pattern (or pattern "{*.nw,*.org}")))
    (rg ss pattern dir)))

(defun ss(ss &optional arg)
  (interactive "Msearch for: \nP")
  (let ((choice (if arg :all :personal)))
    (roy-grep-for-info ss choice)))

(defun ks(ss &optional arg)
  (interactive "Msearch for: \nP")
  (let ((choice (if arg :kust-code :kustomer))
        (pattern (if arg "{*.md,*.js}")))
    (roy-grep-for-info ss choice pattern)))

(defun fs(ss)
  (interactive "Msearch for: ")
  (roy-grep-for-info ss :facebook))


(provide 'init-defuns)
