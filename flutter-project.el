;;; flutter-project.el --- Tools for working with Flutter projects -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/flutter.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; flutter-project.el is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-project.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; flutter-project.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; flutter-project.el provides functions for working with Flutter projects.

;;; Code:

;;; Public interface

(defun flutter-project-get-root ()
  "Find the root of the current project."
  (or (locate-dominating-file default-directory "pubspec.yaml")
      (error "This does not appear to be a Flutter project (pubspec.yaml not found)")))

(defun flutter-project-get-name ()
  "Return the name of the current project."
  (let ((pubspec (concat (flutter-project-get-root) "pubspec.yaml")))
    (with-temp-buffer
      (insert-file-contents pubspec)
      (goto-char 1)
      ;; Pubspec name must be [a-z0-9_]
      ;; https://dart.dev/tools/pub/pubspec#name
      (re-search-forward "^name:[ \t]*\\([a-z][a-z0-9_]+\\)")
      (match-string 1))))

(provide 'flutter-project)
;;; flutter-project.el ends here
