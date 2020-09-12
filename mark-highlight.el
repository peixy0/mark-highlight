;; (bind-key "<f3>" 'mark-highlight-toggle)
;; (bind-key "S-<f3>" 'mark-highlight-reset-universe)

(defface mark-highlight-face-1
  '((t (:background "yellow" :foreground "black"))) "mark-highlight-face-1")

(defface mark-highlight-face-2
  '((t (:background "royal blue" :foreground "black"))) "mark-highlight-face-2")

(defface mark-highlight-face-3
  '((t (:background "rosy brown" :foreground "black"))) "mark-highlight-face-3")

(defface mark-highlight-face-4
  '((t (:background "indian red" :foreground "black"))) "mark-highlight-face-4")

(defface mark-highlight-face-5
  '((t (:background "dark violet" :foreground "black"))) "mark-highlight-face-5")

(defface mark-highlight-face-6
  '((t (:background "maroon" :foreground "black"))) "mark-highlight-face-6")

(defface mark-highlight-face-7
  '((t (:background "slate blue" :foreground "black"))) "mark-highlight-face-7")

(defface mark-highlight-face-8
  '((t (:background "turquoise" :foreground "black"))) "mark-highlight-face-8")

(defcustom mark-highlight-faces '(mark-highlight-face-1
                                  mark-highlight-face-2
                                  mark-highlight-face-3
                                  mark-highlight-face-4
                                  mark-highlight-face-5
                                  mark-highlight-face-6
                                  mark-highlight-face-7
                                  mark-highlight-face-8)
  "Mark Highlight Custom Faces"
  :type '(repeat face))

(setq mark-highlight-current-face 0)

(defun mark-highlight-select-next-face ()
  (setq mark-highlight-current-face (+ mark-highlight-current-face 1))
  (setq mark-highlight-current-face (% mark-highlight-current-face n)))

(setq mark-highlight-managed-symbols (make-hash-table :test 'equal))

(defun mark-highlight-find-overlays-for-symbol (symbol)
  (gethash symbol mark-highlight-managed-symbols))

(defun mark-highlight-add-managed-symbol (symbol overlays)
  (puthash symbol overlays mark-highlight-managed-symbols))

(defun mark-highlight-delete-managed-symbol (symbol)
  (remhash symbol mark-highlight-managed-symbols))

(defun mark-highlight-selected-symbol ()
  (buffer-substring-no-properties (mark) (point)))

(defun mark-highlight-for-all-matched-symbols (symbol f)
  (goto-char (point-min))
  (let ((result '()))
    (while (search-forward symbol nil t)
      (push (funcall f (match-beginning 0) (match-end 0)) result))
    result))

(defun mark-highlight-make-one-overlay (start end symbol face)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'mark-highlight-symbol symbol)
    overlay))

(defun mark-highlight-make-overlays-for-symbol (symbol)
  (let* ((n (length mark-highlight-faces))
         (face (elt mark-highlight-faces mark-highlight-current-face))
         (f (lambda (start end) (mark-highlight-make-one-overlay start end symbol face)))
         (overlays (mark-highlight-for-all-matched-symbols symbol f)))
    (mark-highlight-select-next-face)
    overlays))

(defun mark-highlight-delete-overlays-for-symbol (symbol)
  (mapc 'delete-overlay
        (mark-highlight-find-overlays-for-symbol symbol))
  (mark-highlight-delete-managed-symbol symbol))

(defun mark-highlight-find-symbols-at-point ()
  (seq-filter 'mark-highlight-find-overlays-for-symbol
              (mapcar (lambda (overlay) (overlay-get overlay 'mark-highlight-symbol))
                      (overlays-at (point)))))

(defun mark-highlight-toggle ()
  (interactive)
  (save-excursion
    (save-restriction
      (let ((symbol (mark-highlight-selected-symbol)))
        (if (> (length symbol) 0)
            (if (mark-highlight-find-overlays-for-symbol symbol)
                (mark-highlight-delete-overlays-for-symbol symbol)
              (let* ((overlays (mark-highlight-make-overlays-for-symbol symbol))
                     (n (length overlays)))
                (mark-highlight-add-managed-symbol symbol overlays)
                (message "marked %d occurrence%s" n (if (> n 1) "s" ""))))
          (mapc 'mark-highlight-delete-overlays-for-symbol
                (mark-highlight-find-symbols-at-point)))))))

(defun mark-highlight-reset-universe ()
  (interactive)
  (setq mark-highlight-current-face 0)
  (setq mark-highlight-managed-symbols '())
  (remove-overlays (point-min) (point-max)))

(provide 'mark-highlight)
