;;; flutter.el --- Tools for working with Flutter SDK -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'comint)

(defconst flutter-buffer-name "*Flutter*")

(defvar flutter-sdk-path nil
  "Path to Flutter SDK.")

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
    ("P" . performance-overlay)
    ("s" . screenshot)
    ("q" . quit)))

(defvar flutter-mode-map
  (nconc (make-sparse-keymap) comint-mode-map)
  "Basic mode map for `flutter-run'.")

(defmacro flutter--make-interactive-function (key name)
  "Define a function NAME prefixed with 'flutter-' that sends KEY to the `flutter` process."
  (let ((funcname (intern (concat "flutter-" (symbol-name name)))))
    `(defun ,funcname ()
       ,(format "Send key '%s' to inferior flutter to invoke '%s' function." key name)
       (interactive)
       (flutter--send-command ,key))))

(defmacro flutter-register-key (key name)
  "Register a KEY press with associated NAME recognized by \
`flutter` in interactive mode.  A function `flutter-NAME' will \
be created that sends the key to the `flutter` process."
  `(let ((func (flutter--make-interactive-function ,key ,name)))
    (define-key flutter-mode-map ,key func)))

(defmacro flutter-register-keys (key-alist)
  "Call `flutter-register-key' on all (key . name) pairs in KEY-ALIST."
  `(progn ,@(mapcar (lambda (item)
                      `(flutter-register-key ,(car item) ,(cdr item)))
                    (eval key-alist))))

(flutter-register-keys flutter-interactive-keys-alist)

(defun flutter-build-command ()
  "Build flutter command to execute."
  (let ((bin (when flutter-sdk-path (concat flutter-sdk-path "bin/"))))
    (concat (or bin "") "flutter")))

(defun flutter-get-project-root ()
  "Find the root of the current project."
  (locate-dominating-file default-directory "pubspec.yaml"))

(defmacro flutter--from-project-root (&rest body)
  "Execute BODY with the `default-directory' set to the project root."
  `(let ((root (flutter-get-project-root)))
    (if root
        (let ((default-directory root))
          ,@body)
      (error "Root of Flutter project not found"))))

(defmacro flutter--with-run-proc (&rest body)
  "Execute BODY while ensuring an inferior `flutter` process is running."
  `(flutter--from-project-root
    (let* ((buffer (get-buffer-create flutter-buffer-name))
           (alive (flutter--running-p)))
      (unless alive
        (make-comint-in-buffer "Flutter" buffer (flutter-build-command) nil "run"))
      (with-current-buffer buffer
        (unless (derived-mode-p 'flutter-mode)
          (flutter-mode)))
      ,@body)))

;;;###autoload
(defun flutter-run ()
  "Execute `flutter run` inside Emacs."
  (interactive)
  (flutter--with-run-proc
   (pop-to-buffer-same-window buffer)))

;;;###autoload
(defun flutter-run-or-hot-reload ()
  "Start `flutter run` or hot-reload if already running."
  (interactive)
  (if (flutter--running-p)
      (flutter-hot-reload)
    (flutter-run)))

(defun flutter--running-p ()
  "Return non-nil if an inferior `flutter` process is already running."
  (comint-check-proc flutter-buffer-name))

(defun flutter--send-command (command)
  "Send COMMAND to a running Flutter process."
  (flutter--with-run-proc
   (let ((proc (get-buffer-process flutter-buffer-name)))
     (comint-send-string proc command))))

(defun flutter--initialize ()
  "Helper function to initialize Flutter."
  (setq comint-process-echoes nil))

;;;###autoload
(define-derived-mode flutter-mode comint-mode "Flutter"
  "Major mode for `flutter-run'.

\\{flutter-mode-map}"
  (setq comint-prompt-read-only t))

(add-hook 'flutter-mode-hook #'flutter--initialize)

(provide 'flutter)
;;; flutter.el ends here
