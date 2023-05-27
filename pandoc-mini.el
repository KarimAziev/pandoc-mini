;;; pandoc-mini.el --- Pandoc utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/pandoc-mini
;; Version: 0.1.0
;; Keywords: tools docs convenience
;; Package-Requires: ((emacs "27.1") (transient "0.3.7.50-git"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Pandoc utils

;;; Code:

(require 'transient)

(defcustom pandoc-mini-default-output-formats '((org-mode . "gfm")
                                                (gfm-mode . "org")
                                                (gfm-view-mode . "org")
                                                (markdown-mode . "org")
                                                (html-mode . "org"))
  "Alist of major modes to corresponding pandoc outputs."
  :group 'pandoc-mini
  :type '(alist
          :key-type symbol
          :value-type string))



(defcustom pandoc-mini-executable (executable-find "pandoc")
  "The path to the pandoc executable."
  :group 'pandoc-mini
  :type 'string)

(defcustom pandoc-mini-modes-alist '(("gfm" . gfm-mode)
                                     ("markdown_mmd" . gfm-mode)
                                     ("markdown" . gfm-mode)
                                     ("markdown_strict" . gfm-mode)
                                     ("json" . json-mode)
                                     ("html5" . html-mode)
                                     ("html" . html-mode)
                                     ("html4" . html-mode)
                                     ("org" . org-mode))
  "Alist of output formats to to corresponding modes."
  :group 'pandoc-mini
  :type '(alist
          :key-type string
          :value-type symbol))

(defvar pandoc-mini-preferred-extensions-alist
  '(("html" . ("org" "markdown" "markdown_github"
               "markdown_mmd" "markdown_phpextra" "markdown_strict"))
    ("org" . ("markdown" "markdown" "markdown_github"
              "markdown_mmd" "markdown_phpextra" "markdown_strict"))
    ("md" . ("html" "org" "html5" "markdown_github"
             "gfm" "markdown_phpextra" "markdown_strict")))
  "Alist with of extensions with corresponding pandoc inputs.")

(defvar pandoc-mini-extensions-tranform-alist
  '(("md" . ("gfm" "markdown_mmd" "markdown_phpextra" "markdown_strict")))
  "Alist of extensions with corresponding pandoc inputs.")

(defvar pandoc-mini-output-formats nil)
(defvar pandoc-mini-input-formats nil)
(defvar pandoc-mini-output-styles nil)
(defvar pandoc-mini-file nil)

(defun pandoc-mini-init-formats ()
  "Init formats for pandoc."
  (unless pandoc-mini-input-formats
    (setq pandoc-mini-input-formats
          (process-lines "pandoc" "--list-input-formats")))
  (unless pandoc-mini-output-formats
    (setq pandoc-mini-output-formats
          (process-lines "pandoc" "--list-output-formats")))
  (unless pandoc-mini-output-styles
    (setq pandoc-mini-output-styles
          (process-lines "pandoc" "--list-highlight-styles"))))

(defun pandoc-mini-output-format-to-extension (output-format)
  "Return filename extension for pandoc OUTPUT-FORMAT."
  (or
   (pcase output-format
     ((or "gfm" "markdown" "markdown_github"
          "markdown_mmd"
          "markdown_phpextra"
          "markdown_strict")
      "md")
     ((or "html5" "html" "html4") "html"))
   output-format))



(defun pandoc-mini-outputs-candidates ()
  "Return pandoc output formats for `pandoc-mini-file'."
  (if-let ((ext (and pandoc-mini-file (file-name-extension pandoc-mini-file))))
      (if (assoc ext pandoc-mini-preferred-extensions-alist)
          (let ((priortity-exts
                 (cdr (assoc ext pandoc-mini-preferred-extensions-alist))))
            (seq-sort-by (lambda (ext)
                           (if (member ext priortity-exts)
                               1 -1))
                         '> (remove ext pandoc-mini-output-formats)))
        (remove ext pandoc-mini-output-formats))
    pandoc-mini-output-formats))

(defun pandoc-mini-generate-new-filename (name &optional suffix)
  "Generate new filename by concatenation NAME, SUFFIX and index number.
Default value for SUFFIX is -."
  (unless suffix (setq suffix "-"))
  (let ((ext (file-name-extension name))
        (basename (file-name-base name))
        (parent (file-name-directory name))
        (count)
        (re))
    (setq re (concat suffix "[0-9]+$"))
    (while (file-exists-p (setq name
                                (expand-file-name
                                 (concat basename "." ext)
                                 parent)))
      (setq count (if (string-match-p re basename)
                      (1+ (string-to-number
                           (car (reverse (split-string basename suffix)))))
                    0))
      (setq basename (concat
                      (replace-regexp-in-string
                       re ""
                       basename)
                      suffix
                      (number-to-string count))))
    name))

(defun pandoc-mini-read-outputs ()
  "Read available pandoc outputs."
  (pandoc-mini-init-formats)
  (completing-read "Output: " (pandoc-mini-outputs-candidates)))

(defun pandoc-mini-read-file-name ()
  "Read available pandoc outputs."
  (pandoc-mini-init-formats)
  (read-file-name "File: "))

(defun pandoc-mini-read-choices (prompt choices)
  "Read CHOICES with PROMPT.
If choice includes a function,apply it with rest of choice, otherwise returns
 car of choice (number)."
  (let ((answer (read-multiple-choice
                 prompt
                 choices))
        (func))
    (setq func (seq-find #'functionp answer))
    (if func
        (apply func (seq-rest (memq func answer)))
      (car answer))))


(defun pandoc-mini-confirm-file-if-exist (outfile)
  "Read and perform action for OUTFILE if is existing file.
If OUTFILE is not existing, just return OUTFILE."
  (if  (not (file-exists-p outfile))
      outfile
    (let ((new-name (pandoc-mini-generate-new-filename outfile)))
      (pcase (car (read-multiple-choice
                   (format "File %s already exists. "
                           (file-name-nondirectory outfile))
                   `((?y "Override")
                     (?r ,(format "Rename %s to %s"
                                  (file-name-nondirectory outfile)
                                  (file-name-nondirectory
                                   new-name)))
                     (?a ,(format "Save as %s "
                                  (file-name-nondirectory new-name))))))
        (?y outfile)
        (?r
         (let ((new-name (pandoc-mini-generate-new-filename outfile)))
           (rename-file outfile new-name)))
        (?s (pandoc-mini-generate-new-filename outfile))))))

(defun pandoc-mini-read-extensions (&optional type)
  "Return string with pandoc extensions for pandoc format TYPE."
  (let* ((lines (process-lines "pandoc" (if type
                                            (concat
                                             "--list-extensions="
                                             type)
                                          "--list-extensions")))
         (choices (completing-read-multiple "Extension: "
                                            lines)))
    (mapconcat
     (lambda (it)
       (if (string-match-p "^-" it)
           (concat "+" (substring it 1))
         (concat "-" (substring it 1))))
     choices
     "")))

(defvar-local pandoc-mini-from-extensions nil)
(defvar-local pandoc-mini-to-extensions nil)
(defun pandoc-mini-input-extensions-reader (&rest _)
  "Return string with pandoc extensions for pandoc format TYPE."
  (let* ((val (seq-find (apply-partially #'string-match-p "--from=")
                        (seq-filter #'stringp
                                    (transient-args
                                     transient-current-command))))
         (format-type
          (when val (substring-no-properties
                     val
                     (length "--from=")))))
    (setq pandoc-mini-from-extensions
          (pandoc-mini-read-extensions format-type))))

(defun pandoc-mini-out-extensions-reader (&rest _)
  "Return string with pandoc extensions for pandoc format TYPE."
  (let* ((val (seq-find (apply-partially #'string-match-p "--to=")
                        (seq-filter #'stringp
                                    (transient-args
                                     transient-current-command))))
         (format-type
          (when val (substring-no-properties
                     val
                     (length "--to="))))
         (result (pandoc-mini-read-extensions format-type)))
    (if-let ((cell (assoc format-type pandoc-mini-to-extensions)))
        (setcdr cell result)
      (setq pandoc-mini-to-extensions (push (cons format-type result)
                                            pandoc-mini-to-extensions)))
    (assoc format-type pandoc-mini-to-extensions)))



(defun pandoc-mini-input-format-reader (&optional prompt input history)
  "Read input formats with PROMPT, INPUT and HISTORY."
  (let* ((formats (process-lines "pandoc" "--list-input-formats"))
         (format-type (completing-read (or prompt "Input format: ") formats nil
                                       t input history)))
    format-type))

(defun pandoc-mini-out-format-reader (&optional prompt input history)
  "Read input formats with PROMPT, INPUT and HISTORY."
  (let* ((formats (process-lines "pandoc" "--list-output-formats"))
         (format-type (completing-read (or prompt "Input format: ") formats nil
                                       t input history)))
    format-type))

(transient-define-argument pandoc-mini-input-format-arg ()
  "Input format."
  :class 'transient-option
  :argument "--from="
  :always-read t
  :reader 'pandoc-mini-input-format-reader)

(transient-define-argument pandoc-mini-out-format-arg ()
  "Output format."
  :argument "--to="
  :class 'transient-option
  :reader 'pandoc-mini-out-format-reader)

(transient-define-infix pandoc-mini-output-argument ()
  "Read output filename."
  :argument "--output="
  :class 'transient-files
  :description "Output file"
  :reader 'transient-read-file)

(transient-define-infix pandoc-mini-data-dir-argument ()
  "Read data directory."
  :argument "--data-dir="
  :class 'transient-files
  :reader 'transient-read-existing-directory)

(transient-define-infix pandoc-mini-abbr-argument ()
  "Read data directory."
  :argument "--abbreviations="
  :class 'transient-files
  :reader 'transient-read-existing-file)

(transient-define-infix pandoc-mini-extract-media-argument ()
  "Read data directory for --extract-media."
  :argument "--extract-media="
  :class 'transient-files
  :reader 'transient-read-existing-directory)

(transient-define-argument pandoc-mini-arg-ipynb-output ()
  "Argument for ipynb-output."
  :class 'transient-switches
  :argument-format "--ipynb-output=%s"
  :argument-regexp "\\(--ipynb-output\\(all\\|best\\|none\\)\\)"
  :choices '("all" "none" "best"))

(transient-define-argument pandoc-mini-arg-email-obfuscation ()
  "Argument for email-obfuscation."
  :class 'transient-switches
  :argument-format "--email-obfuscation=%s"
  :argument-regexp
  "\\(--email-obfuscation\\(javascript\\|none\\|references\\)\\)"
  :choices '("none" "javascript" "references"))

(transient-define-argument pandoc-mini-arg-reference-location ()
  "Argument for reference-location."
  :class 'transient-switches
  :argument-format "--reference-location=%s"
  :argument-regexp
  "\\(--reference-location\\(block\\|document\\|section\\)\\)"
  :choices '("block" "section" "document"))

(transient-define-argument pandoc-mini-arg-track-changes ()
  "Argument for track-changes."
  :class 'transient-switches
  :argument-format "--track-changes=%s"
  :argument-regexp "\\(--track-changes=\\(a\\(?:ccept\\|ll\\)\\|reject\\)\\)"
  :choices '("accept" "reject" "all"))

(transient-define-argument pandoc-mini-arg-eol ()
  "Argument for eol."
  :class 'transient-switches
  :argument-format "--eol=%s"
  :argument-regexp "\\(--eol\\(crlf\\|lf\\|native\\)\\)"
  :choices '("crlf" "lf" "native"))

(transient-define-argument pandoc-mini-arg-top-level-division ()
  "Argument for top-level-division."
  :class 'transient-switches
  :argument-format "--top-level-division=%s"
  :argument-regexp "\\(--top-level-division=\\(chapter\\|part\\|section\\)\\)"
  :choices '("section" "chapter" "part"))

(transient-define-argument pandoc-mini-arg-wrap ()
  "Argument for wrap."
  :class 'transient-switches
  :argument-format "--wrap=%s"
  :argument-regexp "\\(--wrap=\\(auto\\|\\(?:non\\|preserv\\)e\\)\\)"
  :choices '("auto" "none" "preserve"))

(transient-define-argument pandoc-mini-toc-depth ()
  "Depth of toc."
  :class 'transient-switches
  :argument-format "--toc-depth=%s"
  :argument-regexp "\\(--toc-depth=\\(0\\|1\\|2\\|3\\|4\\|5\\|6)\\)\\)"
  :choices '("0" "1" "2" "3" "4" "5" "6"))

(transient-define-argument pandoc-mini-highlight-style ()
  "Argument for table of highlight-style."
  :class 'transient-switches
  :argument-format "--highlight-style=%s"
  :argument-regexp
  "\\(--highlight-style=\\(pygments\\|tango\\|espresso\\|zenburn\\|kate\\|monochrome\\|breezedark\\|haddock)\\)\\)"
  :choices '("pygments"
             "tango"
             "espresso"
             "zenburn"
             "kate"
             "monochrome"
             "breezedark"
             "haddock"))

(defun pandoc-mini-read-number (&rest args)
  "Apply `'transient-read-number-N+' with ARGS and format to string."
  (format "%s" (apply #'transient-read-number-N+ args)))

(transient-define-argument pandoc-mini-columns-args ()
  "Argument for table of content depth."
  :class 'transient-option
  :argument "--columns="
  :reader 'pandoc-mini-read-number)

(defun pandoc-mini-get-args ()
	"Return list of sorted arguments of `pandoc-mini-menu'."
	(if (not (eq transient-current-command 'pandoc-mini-menu))
      (transient-args transient-current-command)
    (let* ((args (transient-args transient-current-command))
           (files (car args)))
      (flatten-list (list (remove files args) files)))))

;;;###autoload (autoload 'pandoc-mini-show-args "pandoc-mini.el" nil t)
(transient-define-suffix pandoc-mini-show-args ()
	:transient t
  (interactive)
  (when-let ((args
              (string-join (mapcar (lambda (it)
																		 (if (bufferp it)
																				 (buffer-name it)
																			 it))
																	 (pandoc-mini-get-args)) "\s")))
    (message
     (propertize args 'face 'success))))

;; (transient-define-prefix pandoc-other-options ()
;;   ""
;;   ["Arguments"
;;    ("-o" pandoc-mini-output-argument)
;;    ("D" "data-dir=DIRECTORY" "--data-dir=DIRECTORY")
;;    ("-M" "metadata=KEY[:VALUE]" "--metadata=KEY[:VALUE]")
;;    ("M" "metadata-file=FILE" "--metadata-file=FILE")
;;    ("-d" "defaults=FILE" "--defaults=FILE")
;;    ("tf" "template=FILE" "--template=FILE")
;;    ("-V" "variable=KEY[:VALUE]" "--variable=KEY[:VALUE]")
;;    ("-to" "toc-depth=NUMBER" "--toc-depth=NUMBER")
;;    ("n" "number-offset=NUMBERS" "--number-offset=NUMBERS")
;;    ("r" "resource-path=SEARCHPATH" "--resource-path=SEARCHPATH")
;;    ("-H" "include-in-header=FILE" "--include-in-header=FILE")
;;    ("-B" "include-before-body=FILE" "--include-before-body=FILE")
;;    ("-A" "include-after-body=FILE" "--include-after-body=FILE")
;;    ("s" "syntax-definition=FILE" "--syntax-definition=FILE")
;;    ("d" "dpi=NUMBER" "--dpi=NUMBER")
;;    ("tb" "tab-stop=NUMBER" "--tab-stop=NUMBER")
;;    ("p" "pdf-engine=PROGRAM" "--pdf-engine=PROGRAM")
;;    ("p" "pdf-engine-opt=STRING" "--pdf-engine-opt=STRING")
;;    ("r" "reference-doc=FILE" "--reference-doc=FILE")
;;    ("r" "request-header=NAME:VALUE" "--request-header=NAME:VALUE")
;;    ("i" "indented-code-classes=STRING" "--indented-code-classes=STRING")
;;    ("d" "default-image-extension=extension"
;;     "--default-image-extension=extension")
;;    ("-F" "filter=PROGRAM" "--filter=PROGRAM")
;;    ("-L" "lua-filter=SCRIPTPATH" "--lua-filter=SCRIPTPATH")
;;    ("s" "shift-heading-level-by=NUMBER" "--shift-heading-level-by=NUMBER")
;;    ("b" "base-header-level=NUMBER" "--base-header-level=NUMBER")
;;    ("s" "slide-level=NUMBER" "--slide-level=NUMBER")
;;    ("i" "id-prefix=STRING" "--id-prefix=STRING")
;;    ("-T" "title-prefix=STRING" "--title-prefix=STRING")
;;    ("-c" "css=URL" "--css=URL")
;;    ("e" "epub-subdirectory=DIRNAME" "--epub-subdirectory=DIRNAME")
;;    ("e" "epub-cover-image=FILE" "--epub-cover-image=FILE")
;;    ("e" "epub-metadata=FILE" "--epub-metadata=FILE")
;;    ("e" "epub-embed-font=FILE" "--epub-embed-font=FILE")
;;    ("e" "epub-chapter-level=NUMBER" "--epub-chapter-level=NUMBER")
;;    ("b" "bibliography=FILE" "--bibliography=FILE")
;;    ("c" "csl=FILE" "--csl=FILE")
;;    ("c" "citation-abbreviations=FILE" "--citation-abbreviations=FILE")
;;    ("w" "webtex[=URL]" "--webtex[=URL]")
;;    ("m" "mathjax[=URL]" "--mathjax[=URL]")
;;    ("k" "katex[=URL]" "--katex[=URL]")
;;    ("log" "log=FILE" "--log=FILE")
;;    ("lse" "list-extensions[=FORMAT]" "--list-extensions[=FORMAT]")
;;    ("-D" "print-default-template=FORMAT" "--print-default-template=FORMAT")
;;    ("pd" "print-default-data-file=FILE" "--print-default-data-file=FILE")])


(defun pandoc-mini-read-file (prompt _initial-input _history)
  "PROMPT for file name.

Returns a list containing the filename.  The file must exist."
  (list (file-local-name (expand-file-name
                          (read-file-name prompt nil nil t)))))

(defclass pandoc-mini-input-files-or-buffer (transient-infix)
  ((key         :initform "--")
   (argument    :initform "--")
   (reader      :initform #'pandoc-mini-read-file)
   (always-read :initform t))
  "A transient class to read list of files.
The slot `value' is either a list of files or a single buffer.")

(defun pandoc-mini--get-default-file-list-or-buffer ()
  "Return the default list of files or buffer to print.
In `dired-mode', get the marked files.  In other modes, if a
buffer has a file get the filename, otherwise return the buffer
itself."
  (if (and (derived-mode-p 'dired-mode)
           (fboundp 'dired-get-marked-files))
      (dired-get-marked-files)
    (or (let ((ff (buffer-file-name)))
          (when (and ff (file-readable-p ff))
            (list ff)))
        (current-buffer))))

(transient-define-argument pandoc-mini-file-list-or-buffer-arg ()
  :description "Files"
  :init-value (lambda (obj)
                (setf (slot-value obj 'value)
                      (pandoc-mini--get-default-file-list-or-buffer)))
  :class 'pandoc-mini-input-files-or-buffer)

(cl-defmethod transient-format-value ((obj pandoc-mini-input-files-or-buffer))
  "Format OBJ's value for display and return the result."
  (let ((argument (oref obj argument)))
    (if-let ((value (oref obj value)))
        (propertize
         (if (listp value)
             ;; Should be list of files.
             (mapconcat (lambda (x)
                          (file-relative-name
                           (abbreviate-file-name (string-trim x "\"" "\""))))
                        value " ")
           ;; Should be a buffer
           (prin1-to-string value))
         'face 'transient-value)
      (propertize argument 'face 'transient-inactive-value))))

(defvar-local pandoc-mini-local-args nil)

(defun pandoc-mini-call-in-other-window (func &rest args)
  "Call FUNC with ARGS in the other window."
  (let* ((orig-wind (selected-window))
         (wind-target (if (minibuffer-window-active-p orig-wind)
                          (with-minibuffer-selected-window
                            (let ((wind (selected-window)))
                              (or
                               (window-right wind)
                               (window-left wind)
                               (split-window-right))))
                        (let ((wind (selected-window)))
                          (or
                           (window-right wind)
                           (window-left wind)
                           (split-window-right))))))
    (with-selected-window wind-target
      (apply func args))))

(defvar pandoc-mini-last-out-format nil)
(defvar pandoc-mini-last-out-file nil)
(defun pandoc-mini-results (output)
  "Show OUTPUT in new buffer."
  (if (and pandoc-mini-last-out-file
           (file-exists-p pandoc-mini-last-out-file))
      (progn (find-file-other-window pandoc-mini-last-out-file)
             (setq pandoc-mini-last-out-file nil))
    (let ((pandoc-buff (get-buffer-create (generate-new-buffer-name
                                           "*pandoc-mini-output*")))
          (out-mode
           (cdr-safe
            (assoc
             (when pandoc-mini-last-out-format
               (replace-regexp-in-string "^--to=" ""
                                         pandoc-mini-last-out-format))
             pandoc-mini-modes-alist))))
      (with-current-buffer pandoc-buff
        (when output
          (insert output))
        (when (and out-mode
                   (functionp out-mode))
          (delay-mode-hooks (funcall out-mode)
                            (font-lock-ensure))
          (setq pandoc-mini-last-out-format nil)))
      (unless (get-buffer-window pandoc-buff)
        (with-selected-window (or
                               (window-right
                                (selected-window))
                               (window-left
                                (selected-window))
                               (split-window-right))
          (pop-to-buffer-same-window pandoc-buff))))))

(defun pandoc-mini-run-with-args (command &rest args)
	"Execute COMMAND with ARGS in PROJECT-DIR.
If DIRECTORY doesn't exists, create new.
Invoke CALLBACK without args."
	(let* ((buff-name (generate-new-buffer-name command))
				 (args-buffers (seq-filter 'bufferp args))
         (buffer (get-buffer-create command))
         (proc))
		(setq args (seq-remove 'bufferp args))
    (progn (switch-to-buffer buffer)
           (with-current-buffer buffer
             (setq proc (apply #'start-process buff-name buffer
                               command
                               args))
             (require 'shell)
             (when (fboundp 'shell-mode)
               (shell-mode))
             (view-mode +1))
           (set-process-sentinel
            proc
            (lambda (process _state)
              (let* ((out-buffer (process-buffer
                                  process))
                     (output
											(when out-buffer
                        (with-current-buffer
                            out-buffer
                          (buffer-string)))))
                (if (= (process-exit-status process) 0)
                    (progn (pandoc-mini-results output)
                           (kill-buffer out-buffer))
                  (user-error "%s\n%s" command output)))))
					 (require 'comint)
           (when (fboundp 'comint-output-filter)
             (set-process-filter proc #'comint-output-filter))
					 (dolist (buff args-buffers)
						 (process-send-string proc
																	(with-current-buffer buff (buffer-string))))
					 (when args-buffers
						 (process-send-eof proc)))))

;;;###autoload
(defun pandoc-mini-convert-file ()
  "Convert file with pandoc."
  (interactive)
  (setq pandoc-mini-file (or
                          (when (fboundp 'pandoc-mini-read-file-name)
                            (pandoc-mini-read-file-name))
                          (read-file-name "File to convert:\s"
                                          nil
                                          buffer-file-name
                                          nil
                                          buffer-file-name)))
  (let* ((output-format (when (fboundp 'pandoc-mini-read-outputs)
                          (pandoc-mini-read-outputs)))
         (output-dir (read-directory-name
                      "Directory:\s" nil nil nil
                      (file-name-directory pandoc-mini-file)))
         (outfile (pandoc-mini-confirm-file-if-exist
                   (expand-file-name
                    (concat
                     (file-name-base pandoc-mini-file) "."
                     (pandoc-mini-output-format-to-extension
                      output-format))
                    output-dir)))
         (command)
         (buff))
    (setq command
          (read-string "Run "
                       (string-join
                        `("pandoc" "--standalone -s" ,(shell-quote-argument
                                                       (expand-file-name
                                                        pandoc-mini-file))
                          "-t"
                          ,output-format "-o"
                          ,(shell-quote-argument
                            outfile))
                        "\s")))
    (setq buff (get-buffer-create "*pandoc-mini*"))
    (shell-command command buff buff)
    (if (file-exists-p outfile)
        (find-file outfile)
      (pop-to-buffer buff))))

(defun pandoc-mini-get-arg-value (arg args)
  "Return value of argument ARG from ARGS."
  (when-let ((value (seq-find (lambda (it)
                                (and it
                                     (stringp it)
                                     (string-prefix-p arg it)))
                              args)))
    (substring-no-properties value (length arg))))

;;;###autoload
(defun pandoc-mini-run ()
  "Run quicktype with transient arguments."
  (interactive)
  (when (eq transient-current-command 'pandoc-mini-menu)
    (setq pandoc-mini-local-args
          (transient-args transient-current-command))
    (setq pandoc-mini-last-out-format
          (or (seq-find (apply-partially #'string-match-p "--to=")
                        (seq-filter #'stringp pandoc-mini-local-args))))
    (setq pandoc-mini-last-out-file
          (when-let ((outfile (seq-find (apply-partially #'string-match-p
                                                         "--output=")
                                        (seq-filter #'stringp
                                                    pandoc-mini-local-args))))
            (substring-no-properties outfile (length "--output=")))))
  (apply #'pandoc-mini-run-with-args pandoc-mini-executable
         (pandoc-mini-get-args)))

;;;###autoload (autoload 'pandoc-mini-list-prefix "pandoc-mini.el" nil t)
(transient-define-prefix pandoc-mini-list-prefix ()
  "Pandoc listing options."
  [[("i" "input-formats" "--list-input-formats")
    ("o" "output-formats" "--list-output-formats")
    ("l" "languages" "--list-highlight-languages")
    ("s" "styles" "--list-highlight-styles")]
   ["Print"
    ("f" "print-default-data-file" "--print-default-data-file=FILE")
    ("-D" "print-default-template" "--print-default-template="
     :choices (lambda (&rest _)
                (process-lines "pandoc"
                               "--list-output-formats")))]]
  [("RET" "Run" pandoc-mini-run :transient nil)])

;;;###autoload (autoload 'pandoc-mini-menu "pandoc-mini.el" nil t)
(transient-define-prefix pandoc-mini-menu ()
  "Transient menu for pandoc."
  :man-page "pandoc"
  :info-manual "pandoc"
  :value (lambda ()
           (remove nil
                   (or pandoc-mini-local-args
                       (append
                        (when-let
                            ((to
                              (cdr
                               (assq major-mode
                                     pandoc-mini-default-output-formats))))
                          (list (concat "--to=" to)))
                        (list "--standalone"
                              "--table-of-contents"
                              "--toc-depth=3"
                              "--preserve-tabs"
                              "--eol=lf"
                              "--track-changes=accept"
                              "--trace"
                              "--file-scope"
                              "--wrap=auto"
                              "--columns=72")))))
  ["General"
   (pandoc-mini-file-list-or-buffer-arg)
   ("-o" "Output file" pandoc-mini-output-argument)
   ("-f" "Input format" pandoc-mini-input-format-arg)
   ("-w" "Output format" pandoc-mini-out-format-arg)
   ("-d" "Data dir" pandoc-mini-data-dir-argument)
   ("v" "verbose" "--verbose")
   ("-q" "quiet" "--quiet")
   ("-F" "fail-if-warnings" "--fail-if-warnings")]
  ["Reader options"
   ("-S" "file-scope" "--file-scope")
   ("-p" "preserve-tabs" "--preserve-tabs")
   ("t" "Track changes" pandoc-mini-arg-track-changes)
   ("a" "abbreviations=" pandoc-mini-abbr-argument)
   ("e" "extract-media=" pandoc-mini-extract-media-argument)
   ("E" "trace" "--trace")]
  ["Writer options"
   ("-s" "standalone" "--standalone")
   ("-t" "table-of-contents" "--table-of-contents")
   ("d" "Depth table-of-contents" pandoc-mini-toc-depth)
   ("-a" "ascii" "--ascii")
   ("h" "highligt style" pandoc-mini-highlight-style)
   ("-n" "no-highlight" "--no-highlight")
   ("-i" "incremental" "--incremental")
   ("p" "strip-empty-paragraphs" "--strip-empty-paragraphs")
   ("c" "strip-comments" "--strip-comments")
   ("o" "End of line" pandoc-mini-arg-eol)]
  ["Specific writer options"
   [("-N" "number-sections" "--number-sections")
    ("S" "self-contained" "--self-contained")
    ("s" "reference-links" "--reference-links")
    ("A" "atx-headers" "--atx-headers")
    ("l" "listings" "--listings")
    ("D" "Wrap sections in <section> tags" "--section-divs")
    ("-I" "Id Prefix" "--id-prefix=")
    ("b" "biblatex" "--biblatex")
    ("m" "mathml" "--mathml")]
   [("w" "Wrap" pandoc-mini-arg-wrap)
    ("M" "Columns" pandoc-mini-columns-args)
    ("T" "Top level division "pandoc-mini-arg-top-level-division)
    ("r" "Reference location" pandoc-mini-arg-reference-location)
    ("f" "Email obfuscation" pandoc-mini-arg-email-obfuscation)
    ("i" "Ipynb output" pandoc-mini-arg-ipynb-output)
    ("H" "html-q-tags" "--html-q-tags")
    ("n" "natbib" "--natbib")
    ("g" "gladtex" "--gladtex")]]
  [["Misc"
    ("U" "dump-args" "--dump-args")
    ("I" "ignore-args" "--ignore-args")]
   ["Actions"
    ("L" "List options" pandoc-mini-list-prefix)
    ("C-c C-a" "Show args" pandoc-mini-show-args :transient t)
    ("RET" "Run" pandoc-mini-run)
    ("<return>" "Run" pandoc-mini-run)]]
  (interactive)
  (transient-setup 'pandoc-mini-menu))

(provide 'pandoc-mini)
;;; pandoc-mini.el ends here