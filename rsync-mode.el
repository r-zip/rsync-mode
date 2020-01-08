;;; rsync-mode.el --- rsync projects to remote machines  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Ryan Pilgrim

;; Author: Ryan Pilgrim <ryan.z.pilgrim@gmail.com>
;; URL: https://github.com/r-zip/rsync-mode.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (spinner "1.7.3"))
;; Keywords: remote rsync

;; rsync-mode requires at least GNU Emacs 26.3 and rsync 3.1.3,
;; protocol version 31.

;; rsync-mode is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; rsync-mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with rsync-mode.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; rsync-mode is an interface to the command line tool rsync,
;; implemented as an Emacs package, and oriented toward project-based
;; development. It includes a minor mode and stand-alone commands for
;; running rsync to synchronize a project to single or multiple hosts.

;;; Code:

(require 'spinner)
(require 'time-stamp)

(defvar rsync-local-path nil)
(defvar rsync-remote-paths nil)
(defvar rsync-excluded-dirs nil)
;; to override, delete entry and save in dir-locals
(defvar rsync-default-excluded-dirs '("data"
                                      ".dvc"
                                      ".idea"
                                      ".git"
                                      ".ipynb_checkpoints"
                                      "output"
                                      ".pytest_cache"
                                      "venv"
                                      ".vscode"
                                      "*.egg-info"
                                      ".venv"
                                      "__pycache__"))


(defconst rsync--lighter
  '(" rsync" (:eval (spinner-print rsync--spinner))))

(defun rsync--start-spinner ()
  "Create and start a spinner on this buffer."
  (unless rsync--spinner
    (setq rsync--spinner (spinner-create 'progress-bar-filled t)))
  (spinner-start rsync--spinner))

(define-minor-mode rsync-mode
  "Toggle rsync mode."
  ;; The initial value
  :init-value nil
  ;; The indicator for the mode line
  :lighter rsync--lighter
  :group 'rsync
  (if (not rsync-mode)
      (remove-hook 'after-save-hook #'rsync-all t)
    ;; taken from the spinner readme: https://github.com/Malabarba/spinner.el
    (defvar-local rsync--spinner nil)
    (defvar-local rsync--process nil)
    (add-hook 'after-save-hook #'rsync-all 100 t)))

(defun rsync-get-hostname (path)
  (let ((user-and-hostname (car (split-string path ":"))))
    (if (string-match-p "@" user-and-hostname)
        (cadr (split-string user-and-hostname "@"))
      user-and-hostname)))

(defun rsync-get-excludes (excluded-dirs)
  (if excluded-dirs
      (string-join
       (mapcar
        (lambda (s) (format "--exclude %s" s))
        (delete-dups `(,@excluded-dirs
                       ,@rsync-default-excluded-dirs)))
       " ")
    ""))

(defmacro rsync-with-info (&rest forms)
  (hack-dir-local-variables)
  `(when-let* ((local-path (alist-get 'rsync-local-path file-local-variables-alist))
               (remote-paths (alist-get 'rsync-remote-paths file-local-variables-alist))
               (excluded-dirs (alist-get 'rsync-excluded-dirs file-local-variables-alist))
               (excludes (rsync-get-excludes excluded-dirs)))
     ,@forms))

(defun rsync--get-rsync-buffer-name (remote-path)
  (format "*rsync to %s*"
          (rsync-get-hostname remote-path)))

(defun rsync--process-exit-hook (proc event)
  (spinner-stop rsync--spinner)
  (if (not (string-equal event "finished\n"))
      (message "Rsync process received abnormal event %s" event)
    (message "Rsync complete.")))

(defun rsync-run (remote-path excludes local-path &optional dry-run)
  (rsync--start-spinner)
  (setq rsync--process
        (start-process-shell-command
         "rsync"
         (rsync--get-rsync-buffer-name remote-path)
         (format "rsync %s %s%s %s %s"
                 "-av"
                 (if dry-run "--dry-run " "")
                 excludes
                 local-path
                 remote-path)))
  (with-current-buffer (rsync--get-rsync-buffer-name remote-path)
    (goto-char (point-max))
    (skip-chars-backward "\n[:space:]")
    (insert (concat "\n\n" (time-stamp-string) "\n")))
  (set-process-sentinel rsync--process #'rsync--process-exit-hook))

(defun rsync-all (&optional dry-run)
  (interactive)
  (rsync-with-info
   (when remote-paths
     (dolist (remote-path remote-paths)
       (rsync-run remote-path excludes local-path dry-run)))))

(defun rsync-select-remote (remote)
  (interactive
   (list
    (rsync-with-info
     (ivy-read "Rsync project to: " remote-paths :require-match t))))
  remote)

(defun rsync (&optional dry-run)
  (interactive)
  (let ((selected-remote (call-interactively #'rsync-select-remote)))
    (rsync-with-info
     (rsync-run selected-remote excludes local-path dry-run))))

(provide 'rsync-mode)
;;; rsync-mode.el ends here
