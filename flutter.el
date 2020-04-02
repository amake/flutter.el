;;; flutter.el --- Tools for working with Flutter SDK -*- lexical-binding: t -*-

;; Copyright (C) 2018-2019 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/flutter.el
;; Package-Requires: ((emacs "24.4"))
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

(defvar flutter-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t n")   'flutter-test-current-file)
    (define-key map (kbd "C-c C-t C-n") 'flutter-test-current-file)
    (define-key map (kbd "C-c C-t t")   'flutter-test-at-point)
    (define-key map (kbd "C-c C-t C-t") 'flutter-test-at-point)
    (define-key map (kbd "C-c C-t T")   'flutter-test-all)
    (define-key map (kbd "C-c C-t C-T") 'flutter-test-all)
    map)
  "The keymap used in command `flutter-test-mode' buffers.")

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

(defun flutter--test (&rest args)
  "Execute `flutter test` inside Emacs.

ARGS is a list of CLI flags passed to
`flutter`, and can be nil."
  (flutter--from-project-root
   (compilation-start (format "%s test %s" (flutter-build-command) (mapconcat 'identity args " ")) t)))

;; The second part of the regexp is a translation of this PCRE, which correctly
;; handles escaped quotes:
;;
;; (['\"])(.*?(?<!\\)(?:\\\\)*)\1,
;;
;; Emacs doesn't have negative lookbehind, so the above is reimplemented as:
;;
;; (['\"])(.*[^\\](?:\\\\)*|(?:\\\\)*)\1,
;;
;; This was then translated to the below with the pcre2el package:
;;
;; (rxt-pcre-to-elisp (read-string "regexp: "))
(defconst flutter--test-case-regexp
  (concat "^[ \t]*\\(?:testWidgets\\|test\\|group\\)([\n]*[ \t]*"
          "\\([\"']\\)\\(.*[^\\]\\(?:\\\\\\\\\\)*\\|\\(?:\\\\\\\\\\)*\\)\\1,")
  "Regexp for finding the string title of a test or test group.
The title will be in match 2.")

(defun flutter--find-test-case (line)
  "Search backwards for test name starting at LINE on current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (end-of-line)
    (if (re-search-backward flutter--test-case-regexp nil t)
        (match-string 2))))

(defun flutter--initialize ()
  "Helper function to initialize Flutter."
  (setq comint-process-echoes nil))


;;; Public interface

(defun flutter-build-command ()
  "Build flutter command to execute."
  (let ((bin (when flutter-sdk-path (concat (file-name-as-directory flutter-sdk-path) "bin"))))
    (concat (if bin (file-name-as-directory bin) "") "flutter")))

;;;###autoload
(define-minor-mode flutter-test-mode
  "Toggle Flutter-Test minor mode.
With no argument, this command toggles the mode. Non-null prefix
argument turns on the mode. Null prefix argument turns off the
mode."
  :init-value nil
  :lighter " Flutter-Test"
  :keymap 'flutter-test-mode-map
  :group 'flutter-test)

(defun flutter-test-file-p ()
  "Return non-nil if the current buffer appears to be a test file."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^import 'package:flutter_test/flutter_test.dart';" nil t)))

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
(defun flutter-test-all ()
  "Execute `flutter test` inside Emacs."
  (interactive)
  (flutter--test))

;;;###autoload
(defun flutter-test-current-file ()
  "Execute `flutter test <current-file>` inside Emacs."
  (interactive)
  (let ((test-file (file-relative-name buffer-file-name (flutter-project-get-root))))
    (flutter--test test-file)))

;;;###autoload
(defun flutter-test-at-point ()
  "Execute `flutter test --plain-name <test-name-at-point> <current-file>` inside Emacs."
  (interactive)
  (let ((test-file (file-relative-name buffer-file-name (flutter-project-get-root)))
        (line (line-number-at-pos (point))))
    (flutter--test
     "--plain-name"
     (format "'%s'" (flutter--find-test-case line))
     test-file)))

;;;###autoload
(define-derived-mode flutter-mode comint-mode "Flutter"
  "Major mode for `flutter-run'.

\\{flutter-mode-map}"
  (setq comint-prompt-read-only t))

(add-hook 'flutter-mode-hook #'flutter--initialize)

(provide 'flutter)
;;; flutter.el ends here
