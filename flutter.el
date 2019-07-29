;;; flutter.el --- Tools for working with Flutter SDK -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/flutter.el
;; Package-Requires: ((emacs "24.5"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; flutter.el is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.
;;
;; flutter.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; flutter.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; flutter.el is a package for running the `flutter' binary from the Flutter SDK
;; interactively.  It is most useful when paired with `dart-mode'.

;;; Code:

(require 'comint)
(require 'flutter-project)
(require 'flutter-l10n)

(defconst flutter-buffer-name "*Flutter*")

(defvar flutter-sdk-path nil
  "Path to Flutter SDK.")


;;; Key bindings

(defconst flutter-interactive-keys-alist
  '(("r" . hot-reload)
    ("R" . hot-restart)
    ("h" . help)
    ("w" . widget-hierarchy)
    ("t" . rendering-tree)
    ("L" . layers)
    ("S" . accessibility-traversal-order)
    ("U" . accessibility-inverse-hit-test-order)
    ("i" . inspector)
    ("p" . construction-lines)
    ("o" . operating-systems)
    ("z" . elevation-checker)
    ("P" . performance-overlay)
    ("a" . timeline-events)
    ("s" . screenshot)
    ("d" . detatch)
    ("q" . quit)))

(defvar flutter-mode-map
  (copy-keymap comint-mode-map)
  "Basic mode map for `flutter-run'.")

(defun flutter--make-interactive-function (key name)
  "Define a function that sends KEY to the `flutter` process.
The function's name will be NAME prefixed with 'flutter-'."
  (let* ((name-str (symbol-name name))
         (funcname (intern (concat "flutter-" name-str))))
    (defalias funcname
      `(lambda ()
         ,(format "Send key '%s' to inferior flutter to invoke '%s' function." key name-str)
         (interactive)
         (flutter--send-command ,key)))))

(defun flutter-register-key (key name)
  "Register a KEY with NAME recognized by the `flutter` process.
A function `flutter-NAME' will be created that sends the key to
the `flutter` process."
  (let ((func (flutter--make-interactive-function key name)))
    (define-key flutter-mode-map key func)))

(defun flutter-register-keys (key-alist)
  "Call `flutter-register-key' on all (key . name) pairs in KEY-ALIST."
  (dolist (item key-alist)
    (flutter-register-key (car item) (cdr item))))

(defun flutter-hot-reload ()
  "Dummy to suppress compiler warning.")

(flutter-register-keys flutter-interactive-keys-alist)


;;; Internal utilities

(defmacro flutter--from-project-root (&rest body)
  "Execute BODY with cwd set to the project root."
  `(let ((root (flutter-project-get-root)))
     (if root
         (let ((default-directory root))
           ,@body)
       (error "Root of Flutter project not found"))))

(defmacro flutter--with-run-proc (args &rest body)
  "Execute BODY while ensuring an inferior `flutter` process is running.

ARGS is a space-delimited string of CLI flags passed to
`flutter`, and can be nil."
  `(flutter--from-project-root
    (let* ((buffer (get-buffer-create flutter-buffer-name))
           (alive (flutter--running-p))
           (arglist (if ,args (split-string ,args))))
      (unless alive
        (apply #'make-comint-in-buffer "Flutter" buffer (flutter-build-command) nil "run" arglist))
      (with-current-buffer buffer
        (unless (derived-mode-p 'flutter-mode)
          (flutter-mode)))
      ,@body)))

(defun flutter--running-p ()
  "Return non-nil if the `flutter` process is already running."
  (comint-check-proc flutter-buffer-name))

(defun flutter--send-command (command)
  "Send COMMAND to a running Flutter process."
  (flutter--with-run-proc
   nil
   (let ((proc (get-buffer-process flutter-buffer-name)))
     (comint-send-string proc command))))

(defun flutter--initialize ()
  "Helper function to initialize Flutter."
  (setq comint-process-echoes nil))


;;; Public interface

(defun flutter-build-command ()
  "Build flutter command to execute."
  (let ((bin (when flutter-sdk-path (concat flutter-sdk-path "bin/"))))
    (concat (or bin "") "flutter")))

;;;###autoload
(defun flutter-run (&optional args)
  "Execute `flutter run` inside Emacs.

ARGS is a space-delimited string of CLI flags passed to
`flutter`, and can be nil.  Call with a prefix to be prompted for
args."
  (interactive
   (list (when current-prefix-arg
           (read-string "Args: "))))
  (flutter--with-run-proc
   args
   (pop-to-buffer-same-window buffer)))

;;;###autoload
(defun flutter-run-or-hot-reload ()
  "Start `flutter run` or hot-reload if already running."
  (interactive)
  (if (flutter--running-p)
      (flutter-hot-reload)
    (flutter-run)))

;;;###autoload
(define-derived-mode flutter-mode comint-mode "Flutter"
  "Major mode for `flutter-run'.

\\{flutter-mode-map}"
  (setq comint-prompt-read-only t))

(add-hook 'flutter-mode-hook #'flutter--initialize)

(provide 'flutter)
;;; flutter.el ends here
