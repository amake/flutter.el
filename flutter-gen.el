;;; flutter-gen.el --- Tools for generating Dart code for Flutter -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/flutter.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; flutter-gen.el is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-gen.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; flutter-gen.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; flutter-gen.el provides functions for generating Dart code for Flutter projects.

;;; Code:

(defconst flutter-gen--class-templ
  "class %s extends Equatable {\n%s\n}")

(defconst flutter-gen--prop-decl-templ
  "final %s %s;")

(defconst flutter-gen--ctor-templ
  "const %s({%s}) : super([%s]);")

(defconst flutter-gen--copy-with-templ
  "%s copyWith({%s}) => %s(%s);")

(defconst flutter-gen--merge-templ
  "%s merge(%s other) => copyWith(%s);")

(defun flutter-gen--to-string (obj)
  "Generic to-string function for any OBJ."
  (cond ((stringp obj) obj)
        ((symbolp obj) (symbol-name obj))
        (t (format "%s" obj))))

(defun flutter-gen--property-declarations (properties)
  "Generate property declarations from PROPERTIES."
  (mapconcat (lambda (item)
               (format flutter-gen--prop-decl-templ (cdr item) (car item)))
             properties
             "\n"))

(defun flutter-gen--ctor (name properties)
  "Generate a constructor with NAME and PROPERTIES."
  (let* ((arg-names (mapcar #'car properties))
         (args-init (mapconcat (lambda (name)
                                 (format "this.%s" name))
                               arg-names
                               ", "))
         (args-super (mapconcat #'flutter-gen--to-string arg-names ", ")))
    (format flutter-gen--ctor-templ name args-init args-super)))

(defun flutter-gen--copy-with (name properties)
  "Generate a copyWith implementation for class NAME with PROPERTIES."
  (let ((args-in (mapconcat (lambda (item)
                              (format "%s %s" (cdr item) (car item)))
                            properties
                            ", "))
        (args-out (mapconcat (lambda (item)
                               (let ((name (car item)))
                                 (format "%s: %s ?? this.%s" name name name)))
                             properties
                             ", ")))
    (format flutter-gen--copy-with-templ name args-in name args-out)))

(defun flutter-gen--merge (name properties)
  "Generate a merge implementation for class NAME with PROPERTIES."
  (let ((args-out (mapconcat (lambda (item)
                               (let ((name (car item)))
                                 (format "%s: other.%s" name name)))
                             properties
                             ", ")))
    (format flutter-gen--merge-templ name name args-out)))


;;; Public interface

(defun flutter-gen-value-class (name properties)
  "Generate a value class with NAME and PROPERTIES.

PROPERTIES is an alist of (TYPE . NAME)."
  (let* ((props (flutter-gen--property-declarations properties))
         (ctor (flutter-gen--ctor name properties))
         (copy-with (flutter-gen--copy-with name properties))
         (merge (flutter-gen--merge name properties))
         (body (mapconcat #'identity (list props ctor copy-with merge) "\n\n")))
    (format flutter-gen--class-templ name body)))

(provide 'flutter-gen)
;;; flutter-gen.el ends here
