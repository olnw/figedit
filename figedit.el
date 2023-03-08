;;; figedit.el --- Functions for inserting and editing figures in LaTeX documents. -*- lexical-binding: t -*-

;; Copyright (C) 2023 Oliver Nikolas Winspear

;; Author: Oliver Nikolas Winspear <dev@oliverwinspear.com>
;; Created: 13 Feb 2023
;; Keywords: tools
;; URL: https://github.com/olnw/figedit

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package took inspiration from a blog post by Gilles Castel, in
;; which he describes an efficient workflow for creating and editing
;; mathematical figures using Inkscape.

;; You can read the post here: https://castel.dev/post/lecture-notes-2/

;; This package aims to implement the same functionality in Emacs Lisp.

;;; Code:

(defgroup figedit
  nil
  "Functions for inserting and editing figures in LaTeX documents."
  :group 'tools
  :prefix "figedit-")

(defcustom figedit-template-path "~/Documents/Inkscape Figure Template.svg"
  "Absolute path of the figure template file."
  :type '(file :must-match t) ; Must be the name of an existing file
  :group 'figedit)

(defcustom figedit-root-directory "Figures/"
  "Path to the directory in which figures are stored.
Must be relative to the main TeX source file."
  :type 'string
  :group 'figedit)

(defcustom figedit-file-change-actions '(export-figure build-document)
  "List of actions performed after a figure file is changed while being watched.
Each element of this list must be one of the following symbols:
- `build-document'
- `export-figure'
- `lsp-latex-build'
- `save-buffer'"
  :type 'sexp
  :group 'figedit)

(defcustom figedit-edit-program-open-actions '(save-buffer)
  "List of actions to be performed after `figedit-edit-program' is started.
Each element of this list must be one of the following symbols:
- `build-document'
- `export-figure'
- `lsp-latex-build'
- `save-buffer'"
  :type 'sexp
  :group 'figedit)

(defcustom figedit-edit-program-close-actions '()
  "List of actions to be performed after `figedit-edit-program' is closed.
Each element of this list must be one of the following symbols:
- `build-document'
- `export-figure'
- `lsp-latex-build'
- `save-buffer'"
  :type 'sexp
  :group 'figedit)

(defcustom figedit-edit-program "inkscape"
  "Name of the executable to be used for the creation and editing of figures."
  :type 'string
  :group 'figedit)

(defcustom figedit-build-program "latexmk"
  "Name of the executable to be used for building LaTeX documents."
  :type 'string
  :group 'figedit)

(defcustom figedit-build-args '("-pdf")
  "List of arguments passed to `figedit-build-program' when building a document."
  :type 'sexp
  :group 'figedit)

(defcustom figedit-export-program "inkscape"
  "Name of the executable to be used for exporting figures."
  :type 'string
  :group 'figedit)

(defcustom figedit-export-args '("--export-area-page"
                                 "--export-dpi"
                                 "300"
                                 "--export-type=pdf"
                                 "--export-latex")
  "List of arguments passed to `figedit-export-program' when exporting a figure."
  :type 'sexp
  :group 'figedit)

(defcustom figedit-template-functions (list #'figedit-template-default
                                            #'figedit-template-standalone)
  "List of possible LaTeX template functions to be used by `figedit-insert'.
By default, the first function in this list will be used.  A different template
function can be specified if `figedit-insert' is given a prefix argument."
  :type 'hook
  :group 'figedit)

(defcustom figedit-allowed-extensions '("svg" "pdf" "ps" "eps" "png" "jpg" "jpeg")
  "Whitelist for file extensions shown by `figedit--read-path'.
If nil, allow all extensions."
  :type 'sexp
  :group 'figedit)

(defun figedit--maybe-build-document (action-list)
  ""
  (when (member 'build-document action-list)
    (apply #'start-process
           figedit-build-program
           figedit-build-program
           figedit-build-program
           figedit-build-args))

  (when (and (member 'lsp-latex-build action-list)
             (eq major-mode #'latex-mode))
    (lsp-latex-build)))

(defun figedit--perform-actions (action-list figure-path)
  ""
  (if (member 'export-figure action-list)
      ;; Here, a sentinel is used to only perform
      ;; the build once the export process has completed.
      (figedit-export figure-path
                      (lambda (process event)
                        (unless (process-live-p process)
                          (figedit--maybe-build-document action-list))))
    (figedit--maybe-build-document action-list))

  (when (member 'save-buffer action-list)
    (save-buffer)))

(defun figedit-maybe-make-directory (directory)
  "Check if DIRECTORY exists.  If it does, return t.
If it does not, prompt the user if they would like to create it.
If they say yes, create DIRECTORY and return t.
If they say no, return nil.

The return value of this function reflects whether DIRECTORY exists."
  (or (file-directory-p directory)
      (and (y-or-n-p (format "Directory '%s' not found.  Create it? "
                             directory))
           (not (make-directory directory t)))))

(defun figedit-export (figure-path &optional sentinel)
  "Export the figure file specified by FIGURE-PATH.
This uses Inkscape by default, and creates two new files: a .pdf file
and a .pdf_tex file.  The .pdf file contains the figure without the
text, and the .pdf_tex file contains the LaTeX code needed to place
the text at the right locations on the figure.

If supplied, SENTINEL is used as the sentinel for the export process."
  (set-process-sentinel (apply #'start-process
                               "figedit-export"
                               "figedit-export"
                               figedit-export-program
                               figure-path
                               figedit-export-args)
                        sentinel))

(defun figedit-template-standalone (figure-path)
  "Return the LaTeX code needed to render a standalone figure.
FIGURE-PATH is the path to the figure file.  It can either be absolute, or
relative to your main TeX source file."
  (let ((figure-name-base (file-name-base figure-path))
        (figure-parent-directory (file-name-directory figure-path)))
    (format "\\begin{figure}[ht]
    \\centering
    \\includegraphics[width=\\columnwidth]{%s%s}
    \\caption{%s}
    \\label{fig:%s}
\\end{figure}\n"
            (file-relative-name figure-parent-directory)
            figure-name-base
            figure-name-base
            figure-name-base)))

(defun figedit-template-default (figure-path)
  "Return the LaTeX code needed to render a dual-file PDF+LaTeX figure.
This is the type of figure that is created by `figedit-export'.
FIGURE-PATH is the path to the figure file.  It can either be absolute, or
relative to your main TeX source file.

This template uses the `incfig' command by Gilles Castel, shown below.
I slightly modified it to support figures with an arbitrary parent directory.
Put the following in your LaTeX preamble to use this template:

\\usepackage{import}
\\usepackage{xifthen}
\\usepackage{pdfpages}
\\usepackage{transparent}

\\newcommand{\\incfig}[2]{%
    \\def\\svgwidth{\\columnwidth}
    \\import{#1}{#2.pdf_tex}
}"
  (let ((figure-name-base (file-name-base figure-path))
        (figure-parent-directory (file-name-directory figure-path)))
    (format "\\begin{figure}[ht]
    \\centering
    \\incfig{%s}{%s}
    \\caption{%s}
    \\label{fig:%s}
\\end{figure}\n"
            (file-relative-name figure-parent-directory)
            figure-name-base
            figure-name-base
            figure-name-base)))

(defun figedit--read-path (&optional must-match)
  "Let the user select a figure file, and return its absolute path.

If supplied and non-nil, MUST-MATCH requires the user to enter the
name of an existing file.

Only files with extensions specified by `figedit-allowed-extensions'
will be shown.  If this variable is nil, show all files."
  (expand-file-name
   (read-file-name "Select a figure file: "
                   (expand-file-name figedit-root-directory)
                   nil
                   must-match
                   nil
                   (lambda (file-name)
                     (or (null figedit-allowed-extensions)
                         (file-directory-p file-name)
                         (member (file-name-extension file-name)
                                 figedit-allowed-extensions))))))

;;;###autoload
(defun figedit-edit (figure-path)
  "Open FIGURE-PATH with the program specified by `figedit-edit-program'.
After opening this program, actions in
`figedit-edit-program-open-actions' will be performed.  When it is
closed, actions in `figedit-edit-program-close-actions' will be
performed.  While the figure is being edited, certain actions can be
performed upon every change to the file.  These are specified by
`figedit-file-change-actions'."
  (interactive (list (progn (figedit-maybe-make-directory figedit-root-directory)
                            (figedit--read-path t))))

  (when (file-regular-p figure-path)
    (let ((descriptor (file-notify-add-watch
                       figure-path
                       '(change)
                       (lambda (event)
                         (figedit--perform-actions figedit-file-change-actions figure-path)))))

      ;; Open the figure with the program specified by `figedit-edit-program'.
      ;; If this program is no longer running, stop watching the figure file.
      (set-process-sentinel
       (start-process figedit-edit-program
                      nil
                      figedit-edit-program
                      figure-path)
       (lambda (process event)
         (unless (process-live-p process)
           (figedit--perform-actions figedit-edit-program-close-actions figure-path)
           (file-notify-rm-watch descriptor))))

      (figedit--perform-actions figedit-edit-program-open-actions figure-path))))

;;;###autoload
(defun figedit-insert (figure-path &optional template-function)
  "Insert LaTeX code for FIGURE-PATH figure into current buffer.
FIGURE-PATH is the path to the figure file.  This might be a PDF or
SVG file, for example, but the file type depends on the template
function that is used.  If supplied, TEMPLATE-FUNCTION overrides the
default template function, which is the first element of
`figedit-template-functions'.

If the figure does not already exist, it will be created from the
template file specified by `figedit-template-path'.  Then, it will be
opened in a vector graphics editor, given by `figedit-edit-program'.

After opening this program, actions in
`figedit-edit-program-open-actions' will be performed.  When it is
closed, actions in `figedit-edit-program-close-actions' will be
performed.  While the figure is being edited, certain actions can be
performed upon every change to the file.  These are specified by
`figedit-file-change-actions'."
  (interactive (list (figedit--read-path)
                     (when current-prefix-arg
                       (intern (completing-read "Select a LaTeX template function: "
                                                figedit-template-functions)))))

  (when (figedit-maybe-make-directory figedit-root-directory)
    ;; Make the figure's parent directory if it doesn't already exist.
    (make-directory (file-name-parent-directory figure-path) t)

    ;; Insert the LaTeX code needed to render the figure.
    (insert (funcall (if template-function
                         template-function
                       (car figedit-template-functions))
                     figure-path))

    ;; If the specified figure does not exist, create it from the specified
    ;; template file, open it in the specified figure editor, and watch the
    ;; file for changes.
    (unless (file-exists-p figure-path)
      (copy-file figedit-template-path figure-path)
      (figedit-edit figure-path))))

(provide 'figedit)

;;; figedit.el ends here
