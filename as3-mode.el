;;; as3-mode.el --- A simple mode for editing Actionscript 3 files

;; Copyright (C) 2007  Aemon Cannon

;; Author: Aemon Cannon
;; Keywords: language modes

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;;; Code:

(require 'fdb)
(require 'font-lock)

(defvar as3-flex-livedoc-url "http://livedocs.adobe.com/flash/9.0/ActionScriptLangRefV3/%s.html"
  "The url used to browse to class documentation. See as3-open-livedoc-for-class")

(defvar as3-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" "\""   st)
    (modify-syntax-entry ?\' "\""   st)
    (modify-syntax-entry ?_  "w"    st)
    (modify-syntax-entry ?\\ "\\"   st)
    (modify-syntax-entry ?{ "("   st)
    (modify-syntax-entry ?} ")"   st)
    (modify-syntax-entry ?\[ "("   st)
    (modify-syntax-entry ?\] ")"   st)
    (modify-syntax-entry ?\( "("   st)
    (modify-syntax-entry ?\) ")"   st)
    (modify-syntax-entry ?+  "."    st)
    (modify-syntax-entry ?-  "."    st)
    (modify-syntax-entry ?=  "."    st)
    (modify-syntax-entry ?%  "."    st)
    (modify-syntax-entry ?<  "."    st)
    (modify-syntax-entry ?>  "."    st)
    (modify-syntax-entry ?&  "."    st)
    (modify-syntax-entry ?|  "."    st)
    (modify-syntax-entry ?\240 "."  st)
    (modify-syntax-entry ?/  ". 124b" st)
    (modify-syntax-entry ?*  ". 23"   st)
    (modify-syntax-entry ?\n "> b"  st)
    (modify-syntax-entry ? "> b" st)
    st)
  "Syntax table for `as3-mode'.")

(defvar as3-font-lock-default-face 'as3-font-lock-default-face)

(defconst as3-font-lock-keywords
  (append
   (list
    '("\\<\\(get\\|set\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-type-face)
      (2 font-lock-function-name-face nil t))
    '("\\<\\(function\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
    '("\\<\\([A-Z_]+\\)\\>"
      (1 font-lock-constant-face))
    '("\\<\\([A-Z]\\sw+\\)\\>"
      (1 font-lock-type-face))
    '("\\<\\(var\\)\\>\\(?:\\s-+\\(_?\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
    '("\\(_\\sw+\\)"
      (1 font-lock-variable-name-face nil t))
    '("\\<\\(\\|this\\|super\\|debugger\\|delete\\|export\\|in\\|is\\|typeof\\|with\\)\\>"
      (1 font-lock-builtin-face))
    '("\\<\\(default\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-variable-name-face nil t))
    '("\\<\\(void\\)\\>\\(?:\\s-+\\(\\sw+\\)\\)?"
      (1 font-lock-keyword-face t)
      (2 as3-font-lock-default-face t t))
    '("\\<\\(Infinity\\|NaN\\|undefined\\)\\>" 
      0 font-lock-constant-face t)
    )
   )
  "Subdued level highlighting for As3 mode.")

(defun as3-setup ()
  (set (make-local-variable 'indent-line-function) 'as3-indent-line)
  (setq tab-width 4)
  (as3-project-helper-load))

(define-generic-mode as3-mode
  '("//" ("/*" . "*/"))                 ; Comments
  ;; Keywords
  '("function" "public" "private" "override" "protected" "import" "package"
    "static" "class" "const" "extends" "implements" "var"
    "return" "new" "if" "else" "while" "for" "throw" "default" "void")
  as3-font-lock-keywords                ; Extra font lock
  '("\\.as$")                           ; auto-mode
  '(as3-setup)
  "A major mode for editing Actionscript 3 files.")

;; Indentation

(defun as3-indent-line ()
  "Indent current line of As3 code."
  (interactive)
  (indent-line-to (max 0 (as3-calculate-indentation))))

(defun as3-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (as3-maybe-skip-leading-close-delim)
    (let ((pos (point)))
      (beginning-of-line)
      (if (not (search-backward-regexp "[^\n\t\r ]" 1 0))
	  0
	(progn
	  (as3-maybe-skip-leading-close-delim)
	  (+ (current-indentation) (* 4 (as3-count-scope-depth (point) pos))))))))

(defun as3-maybe-skip-leading-close-delim ()
  (beginning-of-line)
  (forward-to-indentation 0)
  (if (looking-at "\\s)")
      (forward-char)
    (beginning-of-line)))

(defun as3-face-at-point (pos)
  "Return face descriptor for char at point."
  (plist-get (text-properties-at pos) 'face))

(defun as3-count-scope-depth (rstart rend)
  "Return difference between open and close scope delimeters."
  ;;Attempting Steve Yegge's solution..
  ;;  (save-excursion
  ;;    (let ((result (parse-partial-sexp rstart rend)))
  ;;      (if (or (nth 3 result) (nth 4 result) (nth 7 result))
  ;;	  0
  ;;	(nth 0 result)))))
  (save-excursion
    (goto-char rstart)
    (let ((open-count 0)
	  (close-count 0)
	  opoint)
      (while (and (< (point) rend)
		  (progn (setq opoint (point))
			 (re-search-forward "\\s)\\|\\s(" rend t)))
	(if (= opoint (point))
	    (forward-char 1)
	  (cond
	   ;; Don't count if in string or comment.
	   ((as3-face-at-point (- (point) 1))) 
	   ((looking-back "\\s)")
	    (incf close-count))
	   ((looking-back "\\s(")
	    (incf open-count))
	   )))
      (- open-count close-count))))

(defun as3-open-livedoc-for-class ()
  (interactive)
  (let* ((class-name (read-string "Enter full path to class: " (word-at-point)))
	 (livedoc-url as3-flex-livedoc-url)
	 (file-path (replace-regexp-in-string "\\." "/" class-name))
	 (url (format livedoc-url file-path)))
    (browse-url url)))

;; Definitions to support as3 projects
;; 

(defvar as3-project-helper-default-file-name ".as3-mode-project.el"
  "The default project name to search for.")

(defvar as3-project-helper-project-file-path nil
  "Buffer local variable for storing the project file path.
   This variable will be set automatically")
(make-variable-buffer-local 'as3-project-helper-project-file-path)

(defvar as3-project-helper-project-root-dir nil
  "Buffer local variable for storing the project's root directory.
   This variable will be set automatically.")
(make-variable-buffer-local 'as3-project-helper-project-root-dir)

(defvar as3-project-source-paths '()
  "A list of directories containing .as source files for this project.")
(make-variable-buffer-local 'as3-project-source-paths)

(defvar as3-project-flashlog-path "C:/Documents and Settings/acannon/Application Data/Macromedia/Flash Player/Logs/flashlog.txt"
  "The location of the flash log.")
(make-variable-buffer-local 'as3-project-flashlog-path)

(defun as3-project-helper-find-project-file-in-containing-directory (file-name)
  "Starting at the directory containgining file-name, 
   search up the directory tree for a suitable project descriptor to load, return it's path."
  (let* ((dir (file-name-directory file-name))
	 (possible-path (concat dir as3-project-helper-default-file-name)))
    (if (file-directory-p dir)
	(if (file-exists-p possible-path)
	    possible-path
	  (if (not (equal dir (directory-file-name dir)))
	      (as3-project-helper-find-project-file-in-containing-directory (directory-file-name dir)))))))

(defun as3-project-helper-load ()
  "Search up the directory tree for a suitable project descriptor to load for the current buffer."
  (interactive)
  (let ((project-file-path 
	 (as3-project-helper-find-project-file-in-containing-directory buffer-file-name)))
    (if project-file-path
	(progn
	  (setq as3-project-helper-project-file-path project-file-path)
	  (setq as3-project-helper-project-root-dir (file-name-directory project-file-path))
	  (condition-case nil
	      (load project-file-path)
	    (error (message "Crud. Error while loading as3 project file."))))
      (message "Sorry, could not find an as3 project file for this buffer."))))

(defun as3-project-flashlog ()
  "Open a buffer visiting the local system's flash log, as defined by as3-project-flashlog-path."
  (interactive)
  (find-file as3-project-flashlog-path)
  (revert-buffer nil t))

(provide 'as3-mode)
