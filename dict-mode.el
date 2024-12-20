(provide 'dict)

(defun dict-keypress (k)
  (if (dict-edit-ok)
      (progn
        (if (and (= (point) (line-beginning-position)) (not (looking-at " ")))
          (dict-clear-line))
        (if (dict-char-ok-on-line k)
            (let ((inhibit-read-only t))
              (delete-char 1)
              (insert k)
              ))))
  (dict-check-all)
  (if (dict-word-correct) (dict-advance))
)

(defun dict-edit-ok () (< (- (point) (line-beginning-position)) word-size))

(defun dict-erase-ok () (<= (- (point) (line-beginning-position)) word-size))

(defun dict-check-line-paint-only ()
  (if (dict-word-correct)
      (progn
        (if (dict-word-unique)
            (dict-color-line "green")
          (dict-color-line "yellow"))
       )
    (if (dict-line-full)
        (dict-color-line "red")
        (dict-color-line "white")
     ))
)

(defun dict-check-all ()
  (save-excursion
    (goto-char (point-min))
    (while(<= (line-number-at-pos) (length dict-words))
      (dict-check-line-paint-only)
      (forward-line)))
)

(defun dict-clear-line ()
  (interactive nil dict-mode)
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((inhibit-read-only t))
      (delete-char word-size)
      (insert-char ?\s word-size)
      ))
  (goto-char (line-beginning-position)))

(defun dict-clear-all ()
  (save-excursion
    (goto-char (point-min))
    (while(<= (line-number-at-pos) (length dict-words))
      (dict-clear-line)
      (forward-line)))
)

(defun dict-cheat ()
  (interactive nil dict-mode)
  (let ((dict-anas (dict-anagrams (dict-word-at-line))))
    (progn
      (mapc #'message dict-anas)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (line-beginning-position))
          (forward-char word-size)
          (forward-char 3)
          (forward-char word-size)
          (delete-region (point) (line-end-position))
          (mapc #'(lambda (x) (insert-char ?\s) (insert x)) dict-anas))))))

(defun dict-backspace ()
  (interactive nil dict-mode)
  (if (or (not (dict-erase-ok)) (= (point) (point-min)))
      ()
    (if (= (point) (line-beginning-position))
        ((forward-line -1)
         (forward-char word-size)
         )
      )
    (let ((inhibit-read-only t))
      (delete-char -1)
      (save-excursion
        (goto-char (line-beginning-position))
        (forward-char word-size)
        (insert ?\s)
      ))
    (dict-check-all)
    (if (dict-word-correct) (dict-advance))
    )
)

(defun dict-delete ()
  (interactive nil dict-mode)
  (if (not (dict-erase-ok))
      ()
    (if (= (point) (line-end-position))
        ((forward-line)
         )
      )
    (let ((inhibit-read-only t))
      (delete-char 1)
      (save-excursion
        (goto-char (line-beginning-position))
        (forward-char word-size)
        (insert ?\s)
      ))
    (dict-check-all)
    (if (dict-word-correct) (dict-advance))
    )
)

(defun dict-all-good ()
  (let ((all-good t))    
    (save-excursion
      (goto-char (point-min))
      (while (and all-good (not (looking-at "^$")))
        (if (not (and (dict-word-correct) (dict-word-unique)))
            (setq all-good nil))
        (forward-line))
    all-good)))

(defun dict-advance ()
  (interactive nil dict-mode)
  (forward-line)
  (if (dict-all-good)
      (goto-char (point-max))
    (if (looking-at "^$") (goto-char (point-min)))
    (while (and (dict-word-correct) (dict-word-unique))
      (forward-line)
      (if (looking-at "^$") (goto-char (point-min)))
)))

(defun dict-line-full ()
  (= word-size (length (remove ?\s (append (dict-entered-word-at-line) nil)))))

(defun dict-color-line (c)
  (let ((inhibit-read-only t))
    (add-face-text-property
     (line-beginning-position)
     (line-end-position)
     `(:background ,c))
))

(defun dict-list-comp (opp lista listb)
  (if lista
      (if listb
          (if(funcall opp (car lista) (car listb))
              (dict-list-comp opp (cdr lista) (cdr listb))
            (dict-list-comp opp lista (cdr listb))
           )
        nil
       )
    t
))

(defun dict-list-exists (opp list)
  (if list
      (if(funcall opp (car list))
          t
        (dict-list-exists opp (cdr list)))
    nil
  ))

(defun dict-char-ok-on-line (c)
  (dict-char-ok
   (car (append c nil))
   (append (dict-entered-word-at-line) nil)
   (sort (append (dict-word-at-line) nil) '<))
)

(defun dict-char-ok (c words bwords)
  (progn
    (let ((dict-awords (sort (cons c (remove ?\s words)) '<)))
      (dict-list-comp '= dict-awords bwords)
    )
  )
)

; (One of the) correct words at the n:th line
(defun dict-word-at-line ()
  (if
      (>= (- (line-number-at-pos) 1) (length dict-words))
      nil
    (nth (- (line-number-at-pos) 1) dict-words)))

(defun dict-entered-word-at-line ()
  (if (< (- word-size 1) (- (point-max) (line-beginning-position)))
      (buffer-substring-no-properties (line-beginning-position) (+ word-size (line-beginning-position)))
    nil)
)

(defun entered-words ()
  (let ((dict-ews nil))
    (save-excursion
      (goto-char (point-min))
      (while(<= (line-number-at-pos) (length dict-words))
        (setq dict-ews (cons (dict-entered-word-at-line) dict-ews))
        (forward-line)
      ))
    dict-ews
    )
)

(defun entered-relevant-words ()
  (mapcar #'(lambda (x) (save-excursion
                        (goto-line (+ x 1))
                        (dict-entered-word-at-line)
                        ))
    (gethash (dict-sort-word (dict-entered-word-at-line)) dict-word-to-lines))
)

(defun dict-word-correct ()
  (let ((dw (dict-entered-word-at-line)))
    (let ((dws (dict-sort-word dw)))
      (let ((dwls (gethash dws dict-word-to-lines)))
        (when dwls
          (dict-list-exists
           `(lambda (x) (string= x ,dw))
           (mapcar
            `(lambda (x) (gethash x dict-lines-to-word))
            dwls)))))))

(defun dict-word-unique ()
  (= 1
     (length
      (seq-filter
       `(lambda (x) (string= x ,(dict-entered-word-at-line)))
       (entered-relevant-words))))
)

(defun dict-sort-word (w)
  (concat (sort (append w nil) '<))
)

(defun dict-anagrams (w)
  (seq-filter
       (lambda (x) (string= (dict-sort-word x) (dict-sort-word w)))
       dict-words))

(defun dict-mode-variables ()
  (set (make-local-variable 'dict-file-short)
       (let ((insert-default-directory nil))
         (read-file-name "Choose a dictionary segment " "/home/cic/ordlistor/split/")))
  (set (make-local-variable 'dict-file) nil)
  (set (make-local-variable 'dict-words) nil)
  (set (make-local-variable 'word-size) nil)
  (set (make-local-variable 'dict-word-to-lines) nil)
  (set (make-local-variable 'dict-lines-to-word) nil)
)


(define-derived-mode dict-mode special-mode "Dictionary mode"
  "\\<dict-mode-map>Major mode for anagram practice."
  :interactive nil
  (dict-mode-variables)
  (dict-initialise dict-file-short)
)

(defun dict ()
  (interactive)
  (select-window (selected-window))
  (switch-to-buffer "*dict*")
  (dict-mode)
)

(put 'dict-mode 'mode-class 'special)

(add-hook 'dict-mode-hook
          (lambda ()
            (if (string= (buffer-name) "*dict*")
                (progn
                  (setq buffer-read-only t)
                  (let ((inhibit-read-only t)) (erase-buffer))
                  (dict-mode-draw-words)
                  (goto-char (point-min)))
              (message "Dict mode should not be invoked directly"))))

(defun dict-initialise (file-name)
  (setq dict-file-short file-name)
  (setq dict-file (concat "/home/cic/ordlistor/split/" dict-file-short))
  (setq dict-words (dict-mode-read-lines dict-file))
  (setq word-size (length (car dict-words)))
  (setq dict-word-to-lines (make-hash-table :test 'equal :size 100))
  (setq dict-lines-to-word (make-hash-table :test 'equal :size 100))
  (let ((dict-counter 0))
    (mapc #'(lambda (x)
              (let ((x-sort (dict-sort-word x)))
                (progn
                  (if (gethash x-sort dict-word-to-lines)
                      (puthash x-sort
                               (cons dict-counter (gethash x-sort dict-word-to-lines))
                               dict-word-to-lines)
                    (puthash x-sort (list dict-counter) dict-word-to-lines))
                  (puthash dict-counter x dict-lines-to-word)
                  (setq dict-counter (+ dict-counter 1)))))
          dict-words))
  (run-hooks 'dict-mode-hook)
)

(defun dict-add-map (k)
  (define-key dict-mode-map k `(lambda () (interactive) (dict-keypress ,k)))
)

(mapc #'dict-add-map (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "å" "ä" "ö"))

(define-key dict-mode-map (kbd "<backspace>") 'dict-backspace)
(define-key dict-mode-map (kbd "<delete>") 'dict-delete)
(define-key dict-mode-map (kbd "SPC") 'dict-advance)
(define-key dict-mode-map (kbd "RET") 'dict-advance)
(define-key dict-mode-map (kbd "C-c") 'dict-cheat)
(define-key dict-mode-map (kbd "M-n") 'dict-next)
(define-key dict-mode-map (kbd "M-p") 'dict-previous)
(define-key dict-mode-map (kbd "M-s") 'dict-reset)

(defun dict-mode-draw-line (word)
  (let ((inhibit-read-only t))
    (insert-char ?\s word-size)
    (insert (concat " | " (dict-sort-word word)))
    (newline))
)

(defun dict-mode-draw-words ()
  (set-mark nil)
  (mapc #'dict-mode-draw-line dict-words)
)

(defun dict-mode-read-lines (file)
  (with-temp-buffer
    (insert-file-contents file)
    (cl-loop until (eobp)
             collect (prog1 (buffer-substring-no-properties
                             (line-beginning-position)
                             (line-end-position))
                       (forward-line 1)))))

; https://emacs.stackexchange.com/questions/12153/does-some-command-exist-which-goes-to-the-next-file-of-the-current-directory
(defun dict-find-next-file (file &optional backward)
  "Find the next file (by name) in the current directory. With prefix arg, find the previous file."
  (interactive "P")
  (let* ((files (cl-remove-if (lambda (file) (cl-first (file-attributes file)))
                              (sort (directory-files "/home/cic/ordlistor/split/" nil nil t) 'string<)))
           (pos
            (mod (+ (cl-position file files :test 'equal) (if backward -1 1))
                 (length files))))
         (progn
           (message (nth pos files))
           (nth pos files)
           )))

(defun dict-next ()
  (interactive nil dict-mode)
  (setq dict-file-short (dict-find-next-file dict-file-short))
  (dict-initialise dict-file-short)
)

(defun dict-previous ()
  (interactive nil dict-mode)
  (setq dict-file-short (dict-find-next-file dict-file-short t))
  (dict-initialise dict-file-short)
)

(defun dict-reset ()
  (interactive nil dict-mode)
  (message dict-file-short)
  (dict-initialise dict-file-short)
)
