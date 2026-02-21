;;; d2-ts-compile.el --- Compile & preview for d2-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 the d2-ts-mode contributors

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Compile, watch, and preview commands for D2 diagram files.
;;
;; Commands:
;; - `d2-ts-mode-compile'        (C-c C-c) Compile current file
;; - `d2-ts-mode-compile-buffer'  (C-c C-b) Compile buffer contents
;; - `d2-ts-mode-watch'          (C-c C-w) Start live preview in browser
;; - `d2-ts-mode-watch-stop'     (C-c C-q) Stop live preview
;; - `d2-ts-mode-preview'        (C-c C-p) Toggle Emacs image preview

;;; Code:

(defgroup d2-ts-compile nil
  "Compile and preview settings for `d2-ts-mode'."
  :group 'd2-ts
  :prefix "d2-ts-mode-")

(defcustom d2-ts-mode-d2-executable "d2"
  "Path to the d2 executable."
  :type 'string
  :group 'd2-ts-compile)

(defcustom d2-ts-mode-output-format "svg"
  "Output format for d2 compilation."
  :type '(choice (const "svg")
                 (const "png")
                 (const "pdf"))
  :group 'd2-ts-compile)

(defcustom d2-ts-mode-compile-flags nil
  "Additional flags to pass to the d2 CLI."
  :type '(repeat string)
  :group 'd2-ts-compile)

(defcustom d2-ts-mode-watch-method 'emacs
  "Where to display the live preview when using `d2-ts-mode-watch'.
`emacs' suppresses the browser and shows the output image
in an Emacs side window with `auto-revert-mode'.
`browser' lets d2 open a browser tab (d2 default behavior)."
  :type '(choice (const :tag "Emacs side window" emacs)
                 (const :tag "Browser (d2 default)" browser))
  :group 'd2-ts-compile)

(defvar-local d2-ts-compile--watch-process nil
  "The d2 watch process for the current buffer.")

(defvar-local d2-ts-compile--preview-active nil
  "Non-nil when preview mode is active in the current buffer.")

(defvar-local d2-ts-compile--preview-buffer nil
  "The buffer displaying the preview image.")

(defvar-local d2-ts-compile--watch-preview-buffer nil
  "The buffer displaying the watch preview image in Emacs mode.")

;;; Helpers

(defun d2-ts-compile--check-d2 ()
  "Check that the d2 executable is available.
Signal `user-error' if not found."
  (unless (executable-find d2-ts-mode-d2-executable)
    (user-error "D2 executable not found; install from https://d2lang.com/tour/install")))

(defun d2-ts-compile--output-file (input-file)
  "Return the output file path for INPUT-FILE.
Replaces the .d2 extension with the configured output format."
  (concat (file-name-sans-extension input-file)
          "." d2-ts-mode-output-format))

(defun d2-ts-compile--run (input-file output-file &optional callback)
  "Compile INPUT-FILE to OUTPUT-FILE asynchronously.
Call CALLBACK with no arguments on success.
Errors are displayed in the *d2-compile* buffer."
  (let* ((buf (get-buffer-create "*d2-compile*"))
         (args (append d2-ts-mode-compile-flags
                       (list (expand-file-name input-file)
                             (expand-file-name output-file))))
         (proc (make-process
                :name "d2-compile"
                :buffer buf
                :command (cons d2-ts-mode-d2-executable args)
                :sentinel
                (lambda (_process event)
                  (cond
                   ((string-match-p "finished" event)
                    (message "Compiled to %s" output-file)
                    (when callback (funcall callback)))
                   ((string-match-p "\\(?:exited\\|signal\\)" event)
                    (message "d2 compilation failed (see *d2-compile* buffer)")
                    (display-buffer buf)))))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))
    proc))

;;; Interactive commands

;;;###autoload
(defun d2-ts-mode-compile ()
  "Compile the current D2 file.
The output file is placed alongside the source with the extension
determined by `d2-ts-mode-output-format'."
  (interactive)
  (d2-ts-compile--check-d2)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file; use `d2-ts-mode-compile-buffer'"))
  (save-buffer)
  (let* ((input buffer-file-name)
         (output (d2-ts-compile--output-file input)))
    (d2-ts-compile--run input output)))

;;;###autoload
(defun d2-ts-mode-compile-buffer ()
  "Compile the current buffer contents as D2.
Writes buffer contents to a temporary file and compiles it.
Useful for unsaved buffers."
  (interactive)
  (d2-ts-compile--check-d2)
  (d2-ts-compile--run-region (point-min) (point-max)))

;;;###autoload
(defun d2-ts-mode-compile-region (beg end)
  "Compile the region between BEG and END as D2.
Writes the region to a temporary file and compiles it."
  (interactive "r")
  (d2-ts-compile--check-d2)
  (d2-ts-compile--run-region beg end))

(defun d2-ts-compile--run-region (beg end)
  "Compile the region between BEG and END as D2.
Writes it to a temporary file, compiles, and cleans up."
  (let* ((tmp (make-temp-file "d2-" nil ".d2"))
         (output (d2-ts-compile--output-file
                  (or buffer-file-name
                      (expand-file-name "d2-output" temporary-file-directory)))))
    (write-region beg end tmp nil 'silent)
    (d2-ts-compile--run tmp output
                        (lambda () (delete-file tmp)))))

;;;###autoload
(defun d2-ts-mode-watch ()
  "Start d2 in watch mode for the current file.
When `d2-ts-mode-watch-method' is `browser', d2 opens a browser tab.
When it is `emacs', the browser is suppressed and the output image
is displayed in an Emacs side window with `auto-revert-mode'.
If a watch process is already running, it is restarted."
  (interactive)
  (d2-ts-compile--check-d2)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (save-buffer)
  (d2-ts-mode-watch-stop)
  (let* ((input (expand-file-name buffer-file-name))
         (output (expand-file-name (d2-ts-compile--output-file input)))
         (emacs-p (eq d2-ts-mode-watch-method 'emacs))
         (args (append (list "--watch")
                       (when emacs-p '("--browser" "0"))
                       d2-ts-mode-compile-flags
                       (list input output)))
         (source-buf (current-buffer)))
    (setq d2-ts-compile--watch-process
          (make-process
           :name "d2-watch"
           :buffer (get-buffer-create "*d2-watch*")
           :command (cons d2-ts-mode-d2-executable args)
           :sentinel
           (lambda (_process event)
             (when (string-match-p "\\(?:finished\\|exited\\|signal\\)" event)
               (message "d2 watch process stopped")))))
    (when emacs-p
      (run-at-time 1 nil
                   (lambda ()
                     (when (file-exists-p output)
                       (let ((img-buf (find-file-noselect output)))
                         (with-current-buffer img-buf
                           (auto-revert-mode 1))
                         (with-current-buffer source-buf
                           (setq d2-ts-compile--watch-preview-buffer img-buf))
                         (display-buffer img-buf
                                         '(display-buffer-in-side-window
                                           . ((side . right)
                                              (window-width . 0.5)))))))))
    (add-hook 'kill-buffer-hook #'d2-ts-compile--kill-watch nil t)
    (message "d2 watch started for %s (%s)"
             (file-name-nondirectory buffer-file-name)
             (if emacs-p "emacs" "browser"))))

(defun d2-ts-compile--kill-watch ()
  "Stop the watch process when the source buffer is killed."
  (when (and d2-ts-compile--watch-process
             (process-live-p d2-ts-compile--watch-process))
    (delete-process d2-ts-compile--watch-process)))

;;;###autoload
(defun d2-ts-mode-watch-stop ()
  "Stop the d2 watch process for the current buffer.
Also closes the Emacs preview window if it was opened."
  (interactive)
  (if (and d2-ts-compile--watch-process
           (process-live-p d2-ts-compile--watch-process))
      (progn
        (delete-process d2-ts-compile--watch-process)
        (setq d2-ts-compile--watch-process nil)
        (when (and d2-ts-compile--watch-preview-buffer
                   (buffer-live-p d2-ts-compile--watch-preview-buffer))
          (let ((win (get-buffer-window d2-ts-compile--watch-preview-buffer t)))
            (when win
              (delete-window win)))
          (setq d2-ts-compile--watch-preview-buffer nil))
        (message "d2 watch stopped"))
    (message "No d2 watch process running")))

;;;###autoload
(defun d2-ts-mode-preview ()
  "Toggle an Emacs image preview of the current D2 file.
First invocation compiles the file and displays the output image
in a side window with `auto-revert-mode'.  An `after-save-hook'
is added so saving the source automatically recompiles.
Calling again closes the preview and removes the hook."
  (interactive)
  (d2-ts-compile--check-d2)
  (unless buffer-file-name
    (user-error "Buffer is not visiting a file"))
  (if d2-ts-compile--preview-active
      (d2-ts-compile--preview-stop)
    (d2-ts-compile--preview-start)))

(defun d2-ts-compile--preview-start ()
  "Start preview mode: compile, show image, install hooks."
  (save-buffer)
  (let* ((source-buf (current-buffer))
         (input buffer-file-name)
         (output (d2-ts-compile--output-file input)))
    (d2-ts-compile--run
     input output
     (lambda ()
       (let ((img-buf (find-file-noselect output)))
         (with-current-buffer img-buf
           (auto-revert-mode 1))
         (with-current-buffer source-buf
           (setq d2-ts-compile--preview-buffer img-buf))
         (display-buffer img-buf '(display-buffer-in-side-window
                                   . ((side . right)
                                      (window-width . 0.5))))))))
  (add-hook 'after-save-hook #'d2-ts-compile--preview-on-save nil t)
  (setq d2-ts-compile--preview-active t)
  (message "Preview started (C-c C-p to stop)"))

(defun d2-ts-compile--preview-on-save ()
  "Recompile after save when preview is active."
  (when d2-ts-compile--preview-active
    (let* ((input buffer-file-name)
           (output (d2-ts-compile--output-file input)))
      (d2-ts-compile--run input output))))

(defun d2-ts-compile--preview-stop ()
  "Stop preview mode: remove hooks, close preview window."
  (remove-hook 'after-save-hook #'d2-ts-compile--preview-on-save t)
  (when (and d2-ts-compile--preview-buffer
             (buffer-live-p d2-ts-compile--preview-buffer))
    (let ((win (get-buffer-window d2-ts-compile--preview-buffer t)))
      (when win
        (delete-window win))))
  (setq d2-ts-compile--preview-active nil
        d2-ts-compile--preview-buffer nil)
  (message "Preview stopped"))

(provide 'd2-ts-compile)

;;; d2-ts-compile.el ends here
