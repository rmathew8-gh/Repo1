;;; package --- Summary

;;; Code:

(defconst chunk-decl-re "^<<\\(.*?\\)>>=$")
(defconst chunk-use-re "<<\\(.*?\\)>>")

(defun -show ()
  "Show Function."
  (message (format "--%s" (buffer-substring (point) (+ 24 (point))))))

(defmacro with-temp-env (&rest body)
  "Doc here.

  BODY is needed"
  `(let ((fn "/home/roy/git-dir/Antlr/Noweb/test-inputs/complete-sample.nw"))
     (unwind-protect
         (let ((buf "*ert-temp*"))
           (set-buffer (get-buffer-create buf))
           (erase-buffer)
           (insert-file fn)
           ,@body))))


(ert-deftest test-la--prev-chunkname ()
  (with-temp-env
   ;; look backwards.. find chunk.
   (goto-char (point-max)) (should (equal (la--prev-chunkname) "e"))
   ;; now am on chunk line, return same chunk
   (should (equal (la--prev-chunkname) "e"))))

(ert-deftest test-la--next-chunkname ()
  (with-temp-env
   ;; find chunk forward
   (goto-char (point-min)) (should (equal (la--next-chunkname) "a"))
   ;; now am on chunk line, return same chunk
   (should (equal (la--next-chunkname) "a"))))


(ert-deftest test-la--chunk-use-positions ()
  (let ((line1 "The <<quick>> brown <<fox>> jumped quickly.")
        (line2 "The quick brown fox jumped quickly."))
    (should (equal nil (la-chunk-use-positions 101 100 line1)))
    (should (equal nil (la-chunk-use-positions 108 100 line2)))
    (should (equal '((104 113)) (la-chunk-use-positions 108 100 line1)))
    (should (equal nil (la-chunk-use-positions 115 100 line1)))
    (should (equal '((120 127)) (la-chunk-use-positions 125 100 line1)))
    (should (equal nil (la-chunk-use-positions 140 100 line1)))))


(ert-deftest test-la-goto-next-chunk-decl ()
  (with-temp-env
   ;; find chunk forward
   (goto-char (point-min))
   (should (equal (lata-antlr-goto-next-chunk-decl) "a"))
   (should (equal (lata-antlr-goto-next-chunk-decl) "b"))))

(ert-deftest test-la-goto-prev-chunk-decl ()
  (with-temp-env
   ;; find chunk forward
   (goto-char (point-max))
   (should (equal (lata-antlr-goto-prev-chunk-decl) "e"))
   (should (equal (lata-antlr-goto-prev-chunk-decl) "c"))))


(ert-deftest test-la-goto-next-chunk-use ()
  (with-temp-env
   ;; find chunk forward
   (goto-char (point-min))
   (should (equal (lata-antlr-goto-next-chunk-use) "a")) ;; find first chunk (decl)
   (forward-line 1)
   (should (equal (lata-antlr-goto-next-chunk-use) "b")) ;; find 2nd chunk (decl)
   (should (equal (lata-antlr-goto-next-chunk-use) "b")) ;; find chunk (use)
   (should (equal (lata-antlr-goto-next-chunk-use) "b")) ;; should stay stuck on last use.
   (forward-line 1)
   (should (equal (lata-antlr-goto-next-chunk-use) "c")))) ;; find next chunk (use)

(ert-deftest test-la-goto-next-chunk-use-partial ()
  (with-temp-env
   ;; find chunk forward
   (goto-char (point-min))

   ;; first chunk is root chunk; no uses
   (search-forward "<<")
   (should (equal (lata-antlr-goto-next-chunk-use) nil))

   ;; next chunk use is "b"; returns nil after 1 success
   (search-forward "<<")
   (should (equal (lata-antlr-goto-next-chunk-use) "b>>"))
   (should (equal (lata-antlr-goto-next-chunk-use) nil))))

(ert-deftest test-la-goto-prev-chunk-use ()
  (with-temp-env
   ;; find chunk forward
   (goto-char (point-max))
   ;; find chunk forward
   (should (equal (lata-antlr-goto-prev-chunk-use) "e")) ;; find last chunk (decl)
   (forward-line 1)
   (should (equal (lata-antlr-goto-prev-chunk-use) "e")) ;; find earlier chunk (decl)
   (should (equal (lata-antlr-goto-prev-chunk-use) "e")) ;; should stay stuck
   (backward-char 1)
   (should (equal (lata-antlr-goto-prev-chunk-use) "d")))) ;; find prev chunk (use)

;;; lata-antlr-noweb-mode-tests.el ends here

(ert-deftest test-la-goto-prev-chunk-use-partial ()
  (with-temp-env
   (goto-char (point-max))

   ;; first chunk is root chunk; no uses
   (search-backward "e>>")
   (should (equal (lata-antlr-goto-prev-chunk-use) "e>>"))
   (should (equal (lata-antlr-goto-prev-chunk-use) nil))))

