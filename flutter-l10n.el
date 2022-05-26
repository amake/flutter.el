;;; flutter-l10n.el --- Tools for Flutter L10N -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/flutter.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; flutter-l10n.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-l10n.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; flutter-l10n.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; flutter-l10n.el is a package providing helpful functions for localizing
;; Flutter applications according to best practices described at
;; `https://flutter.dev/docs/development/accessibility-and-localization/internationalization'.

;;; Code:

(eval-when-compile (require 'subr-x))
(require 'thingatpt)
(require 'flutter-project)



;;; Public variables

(defvar-local flutter-l10n-conf "l10n.yaml"
  "The project's l10n configuration file.")

(put 'flutter-l10n-conf 'safe-local-variable #'stringp)

(defvar-local flutter-l10n-arb-dir nil
  "The project's l10n directory. Example: \"lib/l10n\"")

(put 'flutter-l10n-arb-dir 'safe-local-variable #'stringp)

(defvar-local flutter-l10n-template-arb-file nil
  "The project's default strings file. Example: \"app_en.arb\"")

(put 'flutter-l10n-template-arb-file 'safe-local-variable #'stringp)

(defvar-local flutter-l10n-output-localization-file nil
  "The project's generated Dart strings file. Example: \"app_localizations.dart\"")

(put 'flutter-l10n-output-localization-file 'safe-local-variable #'stringp)


;;; Code generation

(defconst flutter-l10n--ref-templ "%s.of(context)!.%s")

(defun flutter-l10n--gen-string-ref (id)
  "Generate a reference to the string with ID."
  (format flutter-l10n--ref-templ (flutter-l10n--get-l10n-classname) id))

(defconst flutter-l10n--def-templ-interp
  "\"%s\": \"%s\",\n\"@%s\": {\n  \"placeholders\": {\n  }\n},\n")

(defconst flutter-l10n--def-templ-nointerp
  "\"%s\": \"%s\",\n")

(defun flutter-l10n--gen-string-def (id value)
  "Generate a l10n string definition with ID and VALUE."
  (if (flutter-l10n--has-interp value)
      (format flutter-l10n--def-templ-interp id value id)
    (format flutter-l10n--def-templ-nointerp id value)))

(defun flutter-l10n--has-interp (string)
  "Return non-nil if STRING has interpolation."
  (string-match-p "\\$" string))

(defconst flutter-l10n--comment-templ "// %s")

(defun flutter-l10n--gen-comment (contents)
  "Generate a comment with CONTENTS."
  (format flutter-l10n--comment-templ contents))

(defconst flutter-l10n--import-templ "import 'package:%s/%s';")

(defun flutter-l10n--gen-import (package file)
  "Generate an import statement for FILE in PACKAGE."
  (format flutter-l10n--import-templ package file))


;;; Internal utilities

(defun flutter-l10n--ensure-conf-loaded ()
  "Ensure that l10n conf data is loaded."
  (unless (and flutter-l10n-arb-dir
               flutter-l10n-template-arb-file
               flutter-l10n-output-localization-file)
    (flutter-l10n--load-conf)))

(defun flutter-l10n--load-conf ()
  "Load project config file."
  (let ((conf (concat (file-name-as-directory (flutter-project-get-root)) flutter-l10n-conf)))
    (if (file-readable-p conf)
        (dolist (pair (flutter-l10n--read-yaml conf))
          (cond ((string= (car pair) "arb-dir")
                 (setq-local flutter-l10n-arb-dir (cdr pair)))
                ((string= (car pair) "template-arb-file")
                 (setq-local flutter-l10n-template-arb-file (cdr pair)))
                ((string= (car pair) "output-localization-file")
                 (setq-local flutter-l10n-output-localization-file (cdr pair))))))
    (error "Cannot read flutter l10n conf file at %s" conf)))

(defun flutter-l10n--read-yaml (file)
  "Read YAML-format FILE and return contents as alist."
  (let (result)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char 1)
      (while (re-search-forward "^ *\\([^#][^:]+\\): +\\([^\n]+\\)" nil t)
        (let* ((key (match-string-no-properties 1))
               (value (match-string-no-properties 2)))
          (push `(,key . ,value) result))))
    result))

(defun flutter-l10n--forward-dart-string (&optional arg)
  "Move to the end or beginning of the string at point.
Go forward for positive ARG, or backward for negative ARG.
Assumes start in middle of string.  Not meant for general use;
only for making `bounds-of-thing-at-point' work."
  (interactive "^p")
  (if (natnump arg)
      (re-search-forward "[^\"']+[\"']" nil 'move)
    (re-search-backward "[\"'][^\"']" nil 'move)))

(put 'dart-string 'forward-op #'flutter-l10n--forward-dart-string)

(defun flutter-l10n--normalize-string (string)
  "Normalize a Dart STRING."
  (flutter-l10n--strip-quotes string))

(defun flutter-l10n--strip-quotes (string)
  "Strip qutoes from a quoted STRING."
  (if (string-match-p "^\\([\"']\\).*\\1$" string)
      (substring string 1 -1)
    string))

(defun flutter-l10n--looking-at-import-p ()
  "Return non-nil if current line is an import statement."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "^import ")))

(defun flutter-l10n--get-l10n-file ()
  "Find the root of the project."
  (flutter-l10n--ensure-conf-loaded)
  (concat (flutter-project-get-root)
          (file-name-as-directory flutter-l10n-arb-dir) flutter-l10n-template-arb-file))

(defun flutter-l10n--get-l10n-classname ()
  "The name of the class that has the app's string definitions."
  (flutter-l10n--ensure-conf-loaded)
  (replace-regexp-in-string
   "_" ""
   (capitalize
    (string-remove-suffix ".dart" flutter-l10n-output-localization-file))))

(defun flutter-l10n--l10n-file-exists-p ()
  "Determine if the L10N file exists."
  (file-exists-p (flutter-l10n--get-l10n-file)))

(defun flutter-l10n--append-to-current-line (contents)
  "Append CONTENTS to end of current line."
  (save-excursion
    (end-of-line)
    (insert " " contents)))

(defun flutter-l10n--append-to-l10n-file (definition)
  "Append DEFINITION to the end of the l10n class in the l10n file."
  (let ((target (find-file-noselect (flutter-l10n--get-l10n-file))))
    (with-current-buffer target
      (goto-char (max-char))
      (re-search-backward "}")
      (insert "\n  " definition "\n"))))

(defun flutter-l10n--import-file (package file)
  "Add an import statement for FILE in PACKAGE to the current file."
  (let ((statement (flutter-l10n--gen-import package file)))
    (save-excursion
      (goto-char 1)
      (unless (search-forward statement nil t) ; already imported
        (insert statement "\n")))))

(defun flutter-l10n--import-l10n-file ()
  "Add an import statement for the l10n file to the current file."
  (flutter-l10n--ensure-conf-loaded)
  (flutter-l10n--import-file
   "flutter_gen"
   (concat (file-name-as-directory "gen_l10n")
           flutter-l10n-output-localization-file)))

(defun flutter-l10n--get-existing-ids ()
  "Return a hash table of existing string IDs.
Searches `flutter-l10n-class' in `flutter-l10n-file'.  Values are
t."
  (let ((result (make-hash-table :test #'equal)))
    (if (flutter-l10n--l10n-file-exists-p)
        (with-current-buffer (find-file-noselect (flutter-l10n--get-l10n-file))
          (goto-char 1)
          ;; This relies on a particular whitespace configuration
          ;; TODO: Try to properly parse JSON
          (while (re-search-forward "^[ \t]*\"\\([a-zA-Z0-9_]+\\)\":[ \t]+\".*\"" nil t)
            (puthash (match-string-no-properties 1) t result)))
      ;; Don't `warn' here because it's too intrusive. But with `message' no one
      ;; will notice. TODO: Fix this
      (message "The Flutter L10N file doesn't exist!"))
    result))

(defun flutter-l10n--read-id (existing)
  "Prompt user for a string ID, optionally choosing from EXISTING."
  (let ((response (completing-read "String ID [skip]: "
                                   existing
                                   nil ; predicate
                                   nil ; require-match
                                   nil ; initial-input
                                   nil ; hist
                                   "" ; def
                                   )))
    (if (string-empty-p response)
        nil
      response)))

(defun flutter-l10n--nesting-at-point ()
  "Build a list indicating the nested structure of the code at point.

Each item is of the form (DELIMITER . POSITION), in order of
decreasing position (from leaf to root).  Assumes that code is
well-formed."
  (let (structure
        (curr-point (point)))
    (save-excursion
      (goto-char 1)
      (while (re-search-forward "//\\|[][(){}]" curr-point t)
        (let ((char (match-string-no-properties 0)))
          (cond ((string= "//" char)
                 (end-of-line))
                ((member char '("(" "[" "{"))
                 (push `(,char . ,(match-beginning 0)) structure))
                ((member char '(")" "]" "}"))
                 (pop structure))))))
    structure))

(defun flutter-l10n--find-applied-consts ()
  "Find the `const` keywords that apply to point.

Result is a list of (BEGINNING . END) char positions in
decreasing order (from leaf to root)."
  (let (results
        (structure (flutter-l10n--nesting-at-point)))
    (save-excursion
      (while structure
        (let* ((delim (pop structure))
               (token (car delim))
               (position (cdr delim))
               (bound (cdar structure)))
          (goto-char (- position (length token)))
          (when (and (re-search-backward "\\b[a-z]+\\b" bound t)
                     (string= "const" (match-string 0)))
            ;; TODO: Fix false positive when const in comment
            (push `(,(match-beginning 0) . ,(match-end 0)) results)))))
    (nreverse results)))

(defun flutter-l10n--delete-applied-consts ()
  "Delete the `const` keywords that apply to point."
  (dolist (pos (flutter-l10n--find-applied-consts))
    (delete-region (car pos) (cdr pos))))


;;; Public interface

;;;###autoload
(defun flutter-l10n-externalize-at-point ()
  "Replace a string with a Flutter l10n call.
The corresponding string definition will be put on the kill
ring for yanking into the l10n class."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'dart-string))
         (beg (car bounds))
         (end (cdr bounds))
         (value (flutter-l10n--normalize-string
                 (buffer-substring beg end)))
         (existing (flutter-l10n--get-existing-ids))
         (id (flutter-l10n--read-id existing))
         (definition (flutter-l10n--gen-string-def id value))
         (reference (flutter-l10n--gen-string-ref id))
         (comment (flutter-l10n--gen-comment value)))
    (when id ; null id means user chose to skip
      (delete-region beg end)
      (insert reference)
      (flutter-l10n--delete-applied-consts)
      (flutter-l10n--append-to-current-line comment)
      (flutter-l10n--import-l10n-file)
      (unless (gethash id existing)
        (kill-new definition)))))

;;;###autoload
(defun flutter-l10n-externalize-all ()
  "Interactively externalize all string literals in the buffer.
The corresponding string definitions will be appended to the end
of the l10n class indicated by `flutter-l10n-file'."
  (interactive)
  (let (history
        (existing (flutter-l10n--get-existing-ids)))
    (unwind-protect
        (while (re-search-forward "'[^']*?'\\|\"[^\"]*?\"" nil t)
          ;; Store match bounds now so they don't get clobbered
          (let* ((beg (match-beginning 0))
                 (end (match-end 0))
                 (emptyp (<= (- end beg) 2))) ; Empty match ('' or "")
            (unless (or emptyp
                        (flutter-l10n--looking-at-import-p))
              (push-mark beg)
              (activate-mark)
              (let* ((value (flutter-l10n--normalize-string
                             (match-string 0)))
                     (id (flutter-l10n--read-id existing))
                     (definition (flutter-l10n--gen-string-def id value))
                     (reference (flutter-l10n--gen-string-ref id))
                     (comment (flutter-l10n--gen-comment value)))
                (when id ; null id means user chose to skip
                  ;; `replace-match' sometimes fails with
                  ;; "Match data clobbered by buffer modification hooks"
                  ;; so delete and insert instead. Previously:
                  ;;(replace-match reference t t)
                  (delete-region beg end)
                  (insert reference)
                  (flutter-l10n--delete-applied-consts)
                  (flutter-l10n--append-to-current-line comment)
                  (unless (or (member id history)
                              (gethash id existing))
                    (flutter-l10n--append-to-l10n-file definition))
                  (push id history)
                  (puthash id t existing))))))
      (if history
          (flutter-l10n--import-l10n-file))
      (deactivate-mark))))

(provide 'flutter-l10n)
;;; flutter-l10n.el ends here
