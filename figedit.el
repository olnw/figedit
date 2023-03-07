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

(defcustom figedit-edit-program "inkscape"
  "Name of the executable to be used for the creation and editing of figures."
  :type 'string
  :group 'figedit)

(defcustom figedit-file-change-actions '(export-figure compile-document)
  "List of actions to be performed when a figure file is changed while being watched."
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
  "Whitelist for file extensions showed by `figedit-read-path'.
If nil, allow all extensions."
  :type 'sexp
  :group 'figedit)

(defcustom figedit-export-args '("--export-area-page"
                                 "--export-dpi"
                                 "300"
                                 "--export-type=pdf"
                                 "--export-latex")
  "List of arguments passed to Inkscape when exporting a figure."
  :type 'sexp
  :group 'figedit)

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
  "Export the figure file specified by FIGURE-PATH using Inkscape.
This creates two new files: a .pdf file and a .pdf_tex file.
The .pdf file contains the figure without the text, and the .pdf_tex
file contains the LaTeX code needed to place the text at the right
locations on the figure."
  (set-process-sentinel (apply #'start-process
                               "inkscape-export"
                               nil
                               "inkscape"
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

(defun figedit-maybe-compile-document ()
  "Maybe compile the current LaTeX document using lsp-latex."
  (when (and (member 'compile-document figedit-file-change-actions)
             (eq major-mode #'latex-mode)
             (featurep 'lsp-latex))
    (if (buffer-modified-p)
        (if lsp-latex-build-on-save
            (save-buffer)
          (progn (save-buffer) (lsp-latex-build)))
      (lsp-latex-build))))

(defun figedit-open-and-watch (figure-path)
  "Open FIGURE-PATH with `figedit-edit-program', and watch the file for changes.
Whenever a change is made, perform all actions specified by `figedit-file-change-actions'."
  (when (file-regular-p figure-path)
    (let ((descriptor (file-notify-add-watch
                       figure-path
                       '(change)
                       (lambda (event)
                         (if (member 'export-figure figedit-file-change-actions)

                             ;; Here, a sentinel is used to only perform
                             ;; the build once the export process has completed.
                             (figedit-export figure-path
                                             (lambda (process event)
                                               (unless (process-live-p process)
                                                 (figedit-maybe-compile-document))))
                           (figedit-maybe-compile-document))))))

      ;; Open the figure with the program specified by `figedit-edit-program'.
      ;; If this program is no longer running, stop watching the figure file.
      (set-process-sentinel
       (start-process figedit-edit-program
                      nil
                      figedit-edit-program
                      figure-path)
       (lambda (process event)
         (unless (process-live-p process)
           (file-notify-rm-watch descriptor)))))))

(defun figedit-read-path ()
  "Let the user select a figure file, and return its absolute path.
Only files with extensions specified by `figedit-allowed-extensions'
will be shown.  If this variable is nil, show all files."
  (expand-file-name
   (read-file-name "Select a figure file: "
                   (expand-file-name figedit-root-directory)
                   nil nil nil
                   (lambda (file-name)
                     (or (null figedit-allowed-extensions)
                         (file-directory-p file-name)
                         (member (file-name-extension file-name)
                                 figedit-allowed-extensions))))))

;;;###autoload
(defun figedit-insert ()
  "Let the user select a figure file in `figedit-root-directory' for insertion.
The LaTeX code needed to render this figure will then be inserted into the
current buffer.

If the file does not already exist, it will be created from the template file
specified by `figedit-template-path'.  Then, it will be opened in the program
specified by `figedit-edit-program'.  After any change to the file, the actions
specified by `figedit-file-change-actions' will be performed."
  (interactive)

  (when (figedit-maybe-make-directory figedit-root-directory)
    (let* ((figure-path (figedit-read-path))
           (figure-parent-directory (file-name-parent-directory figure-path))
           (template-function (when current-prefix-arg
                                (intern (completing-read "Select a LaTeX template function: "
                                                         figedit-template-functions)))))

      ;; Make the figure's parent directory if it doesn't exist.
      (unless (file-directory-p figure-parent-directory)
        (make-directory figure-parent-directory t))

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
        (figedit-open-and-watch figure-path)))))

;;;###autoload
(defun figedit-edit ()
  "Let the user select a figure file in `figedit-root-directory' for editing.
The chosen file will be opened in the program specified by
`figedit-edit-program'.  After any change to the file, the actions
specified by `figedit-file-change-actions' will be performed."
  (interactive)

  ;; If the root figure directory does not exist, ask the user if it
  ;; should be created and act accordingly.
  (when (figedit-maybe-make-directory figedit-root-directory)
    (figedit-open-and-watch (figedit-read-path))))

(provide 'figedit)

;;; figedit.el ends here
