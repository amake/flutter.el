;;; flutter-l10n-flycheck.el --- Flycheck checker for intl_translation -*- lexical-binding: t -*-

;; Copyright (C) 2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/flutter.el
;; Package-Requires: ((emacs "26.1") (flycheck "30") (flutter "0.1.0"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; flutter-l10n-flycheck.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; flutter-l10n-flycheck.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; flutter-l10n-flycheck.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; flutter-l10n-flycheck.el is a package providing linting for Flutter
;; localization files using the intl_translation package.

;;; Code:

(require 'flycheck)
(require 'flutter-l10n)

(defun flutter-l10n-flycheck--enable-p ()
  "Determine whether to enable the intl_translation checker."
  (condition-case nil
      (string= (buffer-file-name) (expand-file-name (flutter-l10n--get-l10n-file)))
    (error nil)))

(flycheck-define-checker intl_translation
  "A linter for intl_translation strings files."
  :command ("flutter" "packages" "pub"
            "run" "intl_translation:extract_to_arb"
            source)
  :working-directory (lambda (_) (ignore-errors (flutter-project-get-root)))
  :modes (dart-mode)
  :enabled flutter-l10n-flycheck--enable-p
  :error-patterns
  ((error line-start
          (zero-or-more blank) "reason: " (message) "\n"
          (zero-or-more blank) "from " (file-name) (one-or-more blank)
          "line: " line ", column: " column
          line-end)
   (error line-start
          "Error " (id (one-or-more (not (in ":")))) ":" (message) "\n"
          (zero-or-more any) "\n"
          (zero-or-more any) "\n"
          (zero-or-more blank) "from " (file-name) (one-or-more blank)
          "line: " line ", column: " column
          line-end))
  :next-checkers (lsp-ui))

(defun flutter-l10n-flycheck-setup ()
  "Set up the intl_translation checker."
  (interactive)
  (add-to-list 'flycheck-checkers 'intl_translation))

(provide 'flutter-l10n-flycheck)
;;; flutter-l10n-flycheck.el ends here
