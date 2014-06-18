;;; caballero.el --- Making cabal files work for you

;; Copyright (C) 2014 Philipp Balzarek

;; Author: Philipp Balzarek <p.balzarek@googlemail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(require 'haskell-cabal)
(require 'f)
(require 'cl)

(defgroup caballero nil
  "Haskell cabal files"
  :prefix "caballero/"
  :group 'haskell
)

(defcustom caballero/list-comma-position
  'before
  "Where to put the comma in lists"
  :safe t
  :group 'caballero
  :type '(choice (const before)
                 (const after)))


(modify-syntax-entry ?. "w"  haskell-cabal-mode-syntax-table)
(modify-syntax-entry ?- "w"  haskell-cabal-mode-syntax-table)

(defconst caballero/section-header-regexp "^[[:alnum:]]" )
(defconst caballero/subsection-header-regexp "^[ \t]*[[:alnum:]]\\w*:")
(defconst caballero/comment-regexp "^[ \t]*--")
(defconst caballero/empty-regexp "^[ \t]*$")
(defconst caballero/conditional-regexp "^[ \t]*\\(\\if\\|else\\|}\\)")

(defun caballero/classify-line ()
  "Classify the current line into 'section-header 'subsection-header 'section-data 'comment and 'empty '"
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at caballero/subsection-header-regexp ) 'subsection-header)
     ((looking-at caballero/section-header-regexp) 'section-header)
     ((looking-at caballero/comment-regexp) 'comment)
     ((looking-at caballero/empty-regexp ) 'empty)
     ((looking-at caballero/conditional-regexp ) 'conditional)
     (t 'section-data))))

(defun caballero/header-p ()
  "Is the current line a section or subsection header?"
  (case (caballero/classify-line)
    ((section-header subsection-header) t)))

(defun caballero/section-header-p ()
  "Is the current line a section or subsection header?"
  (case (caballero/classify-line)
    ((section-header) t)))


(defun caballero/section-beginning ()
  "Find the beginning of the current section"
  (save-excursion
    (while (not (or (bobp) (caballero/section-header-p)))
      (forward-line -1))
    (point)))

(defun caballero/beginning-of-section ()
  "go to the beginning of the section"
  (interactive)
  (goto-char (caballero/section-beginning))
)

(defun caballero/section-end ()
  (interactive)
  "Find the end of the current section"
  (save-excursion
    (if  (re-search-forward "\n\\([ \t]*\n\\)*[[:alnum:]]" nil t)
         (match-beginning 0)
         (point-max))))

(defun caballero/end-of-section ()
  "go to the end of the section"
  (interactive)
  (goto-char (caballero/section-end)))

(defun caballero/next-section ()
  "Go to the next extion"
  (interactive)
  (when (caballero/section-header-p) (forward-line))
  (while (not (or (eobp) (caballero/section-header-p)))
    (forward-line)))

(defun caballero/previous-section ()
  "Go to the next extion"
  (interactive)
  (when (caballero/section-header-p) (forward-line -1))
  (while (not (or (bobp) (caballero/section-header-p)))
    (forward-line -1)))

(defun caballero/subsection-end ()
  "find the end of the current subsection"
  (save-excursion
    (caballero/beginning-of-subsection)
    (forward-line)
    (while (and (not (eobp))
                (member (caballero/classify-line) '(empty section-data)))
      (forward-line))
    (unless (eobp) (forward-line -1))
    (while (and (equal  (caballero/classify-line) 'empty)
                (not (bobp)))
      (forward-line -1))
    (end-of-line)
    (point)))

(defun caballero/end-of-subsection ()
  "go to the end of the current subsection"
  (interactive)
  (goto-char (caballero/subsection-end)))

(defun caballero/section ()
  "Get the name and data of the associated section"
  (save-excursion
    (caballero/beginning-of-section)
    (when (looking-at "^\\(\\w+\\):?[ \t]*\\(.*\\)$")
      (list :name (match-string-no-properties 1)
            :value (match-string-no-properties 2)
            :beginning (match-beginning 0)
            :end (caballero/section-end)))))


(defun caballero/subsection ()
  "Get the name and bounds of of the current subsection"
  (save-excursion
    (caballero/beginning-of-subsection)
    (when  (looking-at "\\([ \t]*\\(\\w*\\):\\)[ \t]*")
      (list :name (match-string-no-properties 2)
            :beginning (match-end 0)
            :end (save-match-data (caballero/subsection-end))
            :data-start-column (save-excursion (goto-char (match-end 0))
                                               (current-column)
                                               )))))


(defun caballero/section-name (section)
  (plist-get section :name))

(defun caballero/section-value (section)
  (plist-get section :value))

(defun caballero/section-start (section)
  (plist-get section :beginning))

(defun caballero/section-data-start-column (section)
  (plist-get section :data-start-column))

(defmacro caballero/with-subsection (subsection replace &rest funs)
  "Copy subsection data into a temporary buffer, save indentation
and execute FORMS

If REPLACE is non-nil the subsection data is replaced with the
resultung buffer-content"
  (let ((section (make-symbol "section"))
        (beg (make-symbol "beg"))
        (end (make-symbol "end"))
        (start-col (make-symbol "start-col"))
        (section-data (make-symbol "section-data")))
    `(let* ((,section ,subsection)
            (,beg (plist-get ,section :beginning))
            (,end (plist-get  ,section :end))
            (,start-col (plist-get  ,section :data-start-column))
            (,section-data (buffer-substring ,beg ,end))
            (section-name (plist-get ,section :name )))
       (save-excursion
         (prog1
             (with-temp-buffer
               (indent-to ,start-col)
               (insert ,section-data)
               (beginning-of-buffer)
               (prog1
                   (progn (caballero/save-indentation ,@funs))
                 (beginning-of-buffer)
                 (when (looking-at (format "[ ]\\{0,%d\\}" (1+ ,start-col)))
                     (replace-match ""))

                 (setq ,section-data (buffer-substring (point-min) (point-max)))))
           ,@(when replace
               `((delete-region ,beg ,end)
                 (goto-char ,beg)
                 (insert ,section-data))))))))

(defmacro caballero/each-line (&rest fun)
  "Execute FOMRS on each line"
  `(save-excursion
     (while (< (point) (point-max))
       ,@fun
       (forward-line))))

(defun caballero/chomp-line ()
  "Remove leading and trailing whitespaces from current line"
  (beginning-of-line)
  (when (looking-at "^[ \t]*\\([^ \t]\\|\\(?:[^ \t].*[^ \t]\\)\\)[ \t]*$")
    (replace-match (match-string 1) nil t)
    t))


(defun caballero/min-indentation (&optional beg end)
  "Compute largest common whitespace prefix of each line in between BEG and END"
  (save-excursion
    (goto-char (or beg (point-min)))
    (let ((min-indent nil))
      (while (< (point) (or end (point-max)))
        (let ((indent (current-indentation)))
          (if (and (not (caballero/ignore-line-p))
                   (or (not min-indent)
                       (< indent min-indent)))
              (setq min-indent indent)))
        (forward-line))
      min-indent)))

(defun caballero/ignore-line-p ()
  "Does line only contain whitespaces and comments?"
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*\\(?:--.*\\)?$")))

(defun caballero/kill-indentation ()
  "Remove longest common whitespace prefix from each line"
  (beginning-of-buffer)
  (let ((indent (caballero/min-indentation)))
    (caballero/each-line (unless (caballero/ignore-line-p)
                               (delete-char indent)) )
    indent))

(defun caballero/add-indentation (indent)
  (beginning-of-buffer)
  (caballero/each-line
   (unless (caballero/ignore-line-p)
     (indent-to indent))))


(defmacro caballero/save-indentation (&rest funs)
  "Strip indentation from each line, execute FORMS and reinstate indentation
   so that the indentation of the FIRST LINE matches"
  (let ((old-l1-indent (make-symbol "new-l1-indent"))
        (new-l1-indent (make-symbol "old-l1-indent"))
        (res nil))
    `(let ( (,old-l1-indent (save-excursion
                              (beginning-of-buffer)
                              (current-indentation))))
       (unwind-protect
           (progn
             (caballero/kill-indentation)
             ,@funs)
         (progn
           (beginning-of-buffer)
           (let ((,new-l1-indent (current-indentation)))
             (caballero/add-indentation (- ,old-l1-indent
                                           ,new-l1-indent))))))))

(defun caballero/strip-list ()
  "strip commas from comma-seperated list"
  (beginning-of-buffer)
;; split list items on single line
  (replace-regexp "\\([^ \t,\n]\\)[ \t]*,[ \t]*\\([^ \t,\n]\\)" "\\1\n\\2")
  (beginning-of-buffer)
  (replace-regexp "^\\([ \t]*\\),\\([ \t]*\\)" "")
  (beginning-of-buffer)
  (replace-regexp ",[ \t]*$" "")
  (beginning-of-buffer)
  (caballero/each-line (caballero/chomp-line))
  )

(defun caballero/listify ()
  "Add commas so that buffer contains a comma-seperated list"
  (case caballero/list-comma-position
    ('before
     (beginning-of-buffer)
     (while (caballero/ignore-line-p) (forward-line))
     (indent-to 2)
     (forward-line)
     (caballero/each-line
      (unless (caballero/ignore-line-p)
        (insert ", "))))
    ('after
     (end-of-buffer)
     (while (not (bobp))
       (unless (caballero/ignore-line-p)
         (forward-line -1)
         (end-of-line)
         (insert ",")
         (beginning-of-line))))))



(defmacro caballero/with-cs-list (&rest funs)
  "format buffer so that each line contains a list element "
  `(progn
    (save-excursion (caballero/strip-list))
    (unwind-protect (progn ,@funs)
      (caballero/listify))))


(defun caballero/sort-lines-key-fun ()
  (when (looking-at "[ \t]*--[ \t,]*")
    (goto-char (match-end 0)))
  nil)

(defun caballero/subsection-sort-lines ()
  "Sort lines of current subsection"
  (interactive)
  (caballero/save-position
   (caballero/with-subsection (caballero/subsection) t
                              (caballero/with-cs-list
                               (sort-subr nil 'forward-line 'end-of-line
                                          'caballero/sort-lines-key-fun)))))



(defun caballero/subsection-beginning ()
  "find the beginning of the current subsection"
  (save-excursion
    (while (and (not (bobp))
                (not (caballero/header-p)))
      (forward-line -1))
    (back-to-indentation)
    (point)))

(defun caballero/beginning-of-subsection ()
  "go to the beginniing of the current subsection"
  (interactive)
  (goto-char (caballero/subsection-beginning)))

(defun caballero/next-subsection ()
  "go to the next subsection"
  (interactive)
  (if (caballero/header-p) (forward-line))
  (while (and (not (eobp))
              (not (caballero/header-p)))
    (forward-line))
  (caballero/forward-to-line-entry))

(defun caballero/previous-subsection ()
  "go to the next subsection"
  (interactive)
  (if (caballero/header-p) (forward-line -1))
  (while (and (not (bobp))
              (not (caballero/header-p)))
    (forward-line -1))
  (caballero/forward-to-line-entry)
  )

(defun caballero/find-subsection (section name)
  "Find sunsection with name NAME"
  (save-excursion
    (goto-char (caballero/section-start section))
    (let* ((end (caballero/section-end))
           (found nil))
      (while (and  (< (point) end)
                   (not found))
        (let ((subsection (caballero/subsection)))
          (when (and subsection
                     (string= (downcase (caballero/section-name subsection))
                              (downcase name)))
            (setq found subsection)))
        (caballero/next-subsection))
      found)))

(defun caballero/goto-subsection (name)
  (let ((subsection (caballero/find-subsection (caballero/section) name)))
    (when subsection
      (goto-char (caballero/section-start subsection)))))

(defun caballero/goto-exposed-modules ()
  (interactive)
  (caballero/goto-subsection "exposed-modules"))

(defun caballero/subsection-entry-list (section name)
  "Get the data of a subsection as a list"
  (let ((subsection (caballero/find-subsection section name)))
    (when subsection
      (caballero/with-subsection
       subsection nil
       (caballero/with-cs-list
        (delete-matching-lines
         (format "\\(?:%s\\)\\|\\(?:%s\\)"
                 caballero/comment-regexp
                 caballero/empty-regexp)
         (point-min) (point-max))
        (split-string (buffer-substring-no-properties (point-min) (point-max))
                      "\n" t))))))

(defun caballero/remove-mark ()
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(caballero/marker)))


(defun caballero/mark ()
  "Mark the current position with the text property caballero/marker"
  (caballero/remove-mark)
  (put-text-property (line-beginning-position) (line-end-position)
                     'caballero/marker 'marked-line)
  (put-text-property (point) (1+ (point))
                     'caballero/marker 'marked))


(defun caballero/goto-mark ()
  "Go to marked line"
  (let ((marked-pos (text-property-any (point-min) (point-max)
                                       'caballero/marker
                                       'marked))
        (marked-line (text-property-any (point-min) (point-max)
                                       'caballero/marker
                                       'marked-line) )
        )
    (cond (marked-pos (goto-char marked-pos))
          (marked-line (goto-char marked-line)))))

(defmacro caballero/save-position (&rest forms)
  "Save position as mark, execute FORMs and go back to mark"
  `(prog2
       (caballero/mark)
       (progn ,@forms)
     (caballero/goto-mark)
     (caballero/remove-mark)))


(defmacro caballero/with-subsection-line (replace &rest forms)
  "Mark line and "
  `(progn
     (caballero/mark)
     (unwind-protect
         (caballero/with-subsection (caballero/subsection) ,replace
          (caballero/goto-mark)
          ,@forms)
       (caballero/remove-mark))))


(defun caballero/get-line-content ()
  (caballero/with-subsection-line
   nil
   (caballero/with-cs-list
    (caballero/goto-mark)
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position)))))

(defun caballero/module-to-filename (module)
  (concat  (replace-regexp-in-string "[.]" "/" module ) ".hs"))

(defconst caballero/module-sections '("exposed-modules" "other-modules")
  "List of sections that contain module names"
)

(defconst caballero/file-sections
  '("main-is" "c-sources" "data-files" "extra-source-files"
    "extra-doc-files" "extra-tmp-files" )
  "List of sections that contain filename"
  )



(defun caballero/line-filename ()
  "Expand filename in current line according to the subsection type

Module names in exposed-modules and other-modules are expanded by replacing each dot (.) in the module name with a foward slash (/) and appending \".hs\"

Example: Foo.Bar.Quux ==> Foo/Bar/Quux.hs

Source names from main-is and c-sources sections are left untouched

"
  (let ((entry (caballero/get-line-content))
        (subsection (downcase (caballero/section-name
                               (caballero/subsection)))))
    (cond ((member subsection caballero/module-sections)
           (caballero/module-to-filename entry))
          ((member subsection caballero/file-sections) entry))))

(defun caballero/find-or-create-source-file ()
  "Open the source file this line refers to"
  (interactive)
  (let* ((src-dirs (append (caballero/subsection-entry-list
                            (caballero/section) "hs-source-dirs")
                           '("")))
         (base-dir (file-name-directory (buffer-file-name)))
         (filename (caballero/line-filename)))
    (when filename
      (let ((candidates
             (delq nil (mapcar
                        (lambda (dir)
                          (let ((file (f-join base-dir dir filename)))
                            (when (and (file-readable-p file)
                                       (not (file-directory-p file)))
                              file)))
                        src-dirs))))
        (if (null candidates)
            (let* ((src-dir (f-join base-dir (or (car src-dirs) "")))
                   (newfile (f-join src-dir filename))
                   (subdir (file-name-directory newfile))
                   (do-create-p (y-or-n-p (format "Create file %s ?" newfile))))
              (when do-create-p
                (find-file-other-window newfile )))
          (find-file-other-window (car candidates)))))))


(defun caballero/find-section-type (type &optional wrap)
  (save-excursion
    (caballero/next-section)
    (while
        (not
         (or
          (eobp)
          (string=
           (downcase type)
           (downcase  (caballero/section-name (caballero/section))))))
      (caballero/next-section))
    (if (eobp)
      (if wrap (progn
                 (beginning-of-buffer)
                 (caballero/find-section-type type nil) )
        nil)
      (point))))

(defun caballero/goto-section-type  (type)
  (let ((section (caballero/find-section-type type t)))
    (if section (goto-char section)
      (message "No %s section found" section-name))))

(defun caballero/goto-library-section ()
  (interactive)
  (caballero/goto-section-type "library"))

(defun caballero/goto-test-suite-section ()
  (interactive)
  (caballero/goto-section-type "test-suite"))

(defun caballero/goto-executable-section ()
  (interactive)
  (caballero/goto-section-type "executable"))

(defun caballero/goto-benchmark-section ()
  (interactive)
  (caballero/goto-section-type "benchmark"))


(define-key haskell-cabal-mode-map  (kbd "C-c s")
  'caballero/subsection-sort-lines)
(define-key haskell-cabal-mode-map  (kbd "C-M-n") 'caballero/next-section)
(define-key haskell-cabal-mode-map  (kbd "C-M-p") 'caballero/previous-section)
(define-key haskell-cabal-mode-map  (kbd "M-n") 'caballero/next-subsection)
(define-key haskell-cabal-mode-map  (kbd "M-p")
  'caballero/previous-subsection)
(define-key haskell-cabal-mode-map  (kbd "C-<down>")
  'caballero/next-subsection)
(define-key haskell-cabal-mode-map  (kbd "C-<up>")
  'caballero/previous-subsection)
(define-key haskell-cabal-mode-map  (kbd "C-c f")
  'caballero/find-or-create-source-file)
(define-key haskell-cabal-mode-map  (kbd "M-g l")
  'caballero/goto-library-section)
(define-key haskell-cabal-mode-map  (kbd "M-g e")
  'caballero/goto-executable-section)
(define-key haskell-cabal-mode-map  (kbd "M-g b")
  'caballero/goto-benchmark-section)
(define-key haskell-cabal-mode-map  (kbd "M-g t")
  'caballero/goto-test-suite-section)

(defun caballero/line-entry-column ()
  "Column at which the line entry starts"
  (save-excursion
    (case (caballero/classify-line)
      (section-data (beginning-of-line)
                    (when (looking-at "[ ]*\\(?:,[ ]*\\)?")
                      (goto-char  (match-end 0))
                      (current-column)))
      (subsection-header
       (caballero/section-data-start-column (caballero/subsection))))))

(defun caballero/forward-to-line-entry ()
  "go forward to the beginning of the line entry (but never move backwards)"
  (let ((col (caballero/line-entry-column)))
    (when (and col (< (current-column) col))
      (beginning-of-line)
      (forward-char col))))

(defun caballero/indent-line ()
  "Indent current line according to subsection"
  (interactive)
  (case (caballero/classify-line)
    (section-data
     (save-excursion
       (let ((indent (caballero/section-data-start-column
                      (caballero/subsection))))
         (indent-line-to indent)
         (beginning-of-line)
         (when (looking-at "[ ]*\\([ ]\\{2\\},[ ]*\\)")
           (replace-match ", " t t nil 1))))))
  (caballero/forward-to-line-entry))

(add-hook
 'haskell-cabal-mode-hook
 #'(lambda ()
     (set (make-variable-buffer-local 'indent-line-function)
          'caballero/indent-line)))




(provide 'caballero)
