;;; rsync-mode.el --- Rsync projects to remote machines  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020-2021 Ryan Pilgrim

;; Author: Ryan Pilgrim <ryan.z.pilgrim@gmail.com>
;; URL: https://github.com/r-zip/rsync-mode.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3") (spinner "1.7.1"))
;; Keywords: comm

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
(require 'subr-x)

(defgroup rsync nil
  "Convenient remote synchronization."
  :group 'convenience
  :prefix "rsync-"
  :link '(url-link "https://github.com/r-zip/rsync-mode"))

(defvar rsync-local-path nil
  "Local path to the project, as a string.")

(defvar rsync-remote-paths nil
  "Remote paths to the project as a list of strings.
Each path should have the form 'host:/path/to/project'.")

(defvar rsync-excluded-dirs nil)

;; to override, delete entry and save in dir-locals
(defcustom rsync-default-excluded-dirs nil
  "List of directories to exclude from all projects for rsync."
  :group 'rsync
  :type (list 'string))

(defcustom rsync-sync-on-save nil
  "Whether to activate a hook that synchronizes the project after each save."
  :group 'rsync
  :type 'boolean)

(defvar-local rsync-mode nil
  "Whether rsync-mode is enabled.")

(defvar-local rsync--process nil
  "Rsync process object.")

(defvar-local rsync--spinner nil
  "Rsync spinner object.")

(defvar rsync--process-exit-hook nil
  "Closure defining the process cleanup code.")

(defconst rsync--lighter
  '(" rsync" (:eval (spinner-print rsync--spinner)))
  "The mode lighter.")

(defvar rsync-local-path nil
  "The path to the local repository to be rsync'ed to the remote.")

(defvar rsync-remote-paths nil
  "The paths to the remote repositories.
These must have the form hostname:path/to/repo (relative or absolute).")

(defun rsync--start-spinner ()
  "Create and start a spinner on this buffer."
  (when rsync-mode
    (unless rsync--spinner
      (setq rsync--spinner (spinner-create 'progress-bar-filled t)))
    (spinner-start rsync--spinner)))

(define-minor-mode rsync-mode
  "Toggle rsync mode."
  ;; The initial value
  :init-value nil
  ;; The indicator for the mode line
  :lighter rsync--lighter
  :group 'rsync
  (if (not rsync-remote-paths)
      (message "Failed to activate rsync-mode: No remote configuration for rsync-mode found in dir-locals.")
    (if (not rsync-mode)
        (remove-hook 'after-save-hook #'rsync-all t)
      ;; taken from the spinner readme: https://github.com/Malabarba/spinner.el
      (setq rsync--spinner nil)
      (setq rsync--process nil)
      (when rsync-sync-on-save
        (add-hook 'after-save-hook #'rsync-all 0 t)))))

(defun rsync--get-hostname (path)
  "Get the hostname from the remote path PATH."
  (let ((user-and-hostname (car (split-string path ":"))))
    (if (string-match-p "@" user-and-hostname)
        (cadr (split-string user-and-hostname "@"))
      user-and-hostname)))

(defun rsync--get-excludes (excluded-dirs)
  "Get rsync exclude flags for EXCLUDED-DIRS.
EXCLUDED-DIRS should be a list of strings."
  (if excluded-dirs
      (string-join
       (mapcar
        (lambda (s) (format "--exclude %s" s))
        (delete-dups `(,@excluded-dirs
                       ,@rsync-default-excluded-dirs)))
       " ")
    ""))

(defun rsync--get-rsync-buffer-name (remote-path)
  "Generate the buffer name for the rsync process.
REMOTE-PATH is the path to the rsync destination."
  (format "*rsync to %s*"
          (rsync--get-hostname remote-path)))

(defun rsync--run-process-exit-hook (proc event)
  "Run the closure defined by the variable `rsync--process-exit-hook'.
PROC is the rsync process, which is present for call signature
compatibility only. EVENT is the description of the event that
changed the state of the rsync process."
  (funcall rsync--process-exit-hook proc event))

(defun rsync--make-process-exit-hook (buffer)
  "Create function to clean up the spinner for BUFFER.
The created function will also message the user when the rsync
process is complete and forward abnormal event strings."
  (lambda (_ event)
    (with-current-buffer buffer
      (when rsync-mode
        (spinner-stop rsync--spinner)))
    (if (not (string-equal event "finished\n"))
        (message "Rsync process received abnormal event %s" event)
      (message "Rsync complete."))))

(defun rsync--run (remote-path excludes local-path &optional dry-run file)
  "Synchronize the current project from LOCAL-PATH to REMOTE-PATH.
Exclude according to EXCLUDES and the variable
`rsync-default-excluded-dirs'. If DRY-RUN is t, call rsync with
the dry-run flag.

If FILE is non-nil, sync only that file. The path specified
by FILE is assumed to be relative to LOCAL-PATH."
  (rsync--start-spinner)
  (setq rsync--process
        (start-process-shell-command
         "rsync"
         (rsync--get-rsync-buffer-name remote-path)
         (format "rsync %s %s%s %s %s"
                 (if file "-avR" "-av")
                 (if dry-run "--dry-run " "")
                 excludes
                 (if file (concat local-path "/./" file) local-path)
                 remote-path)))
  (with-current-buffer (rsync--get-rsync-buffer-name remote-path)
    (goto-char (point-max))
    (skip-chars-backward "\n[:space:]")
    (insert (concat "\n\n" (time-stamp-string) "\n")))
  (setq rsync--process-exit-hook (rsync--make-process-exit-hook (current-buffer)))
  (set-process-sentinel rsync--process #'rsync--run-process-exit-hook))

(defun rsync-all (&optional dry-run file)
  "Synchronize the current project to all remote hosts.
If DRY-RUN is t, call rsync with the dry-run flag.

If FILE is non-nil, sync only that file. The path specified
by FILE is assumed to be relative to RSYNC-LOCAL-PATH."
  (interactive)
  (when rsync-remote-paths
    (dolist (remote-path rsync-remote-paths)
      (rsync--run remote-path (rsync--get-excludes rsync-excluded-dirs) rsync-local-path dry-run file))))

(defun rsync--select-remote ()
  "Interactively select the remote for synchronization.
REMOTE is the selected remote host."
  (interactive)
  (completing-read "Rsync project to: " rsync-remote-paths nil t))

(defun rsync (&optional dry-run file)
  "Synchronize the current project to a single remote host.
The host is selected interactively by the function
`rsync--select-remote'. If DRY-RUN is t, call rsync with the
dry-run flag.

If FILE is non-nil, sync only that file. The path specified
by FILE is assumed to be relative to RSYNC-LOCAL-PATH."
  (interactive)
  (let ((selected-remote (call-interactively #'rsync--select-remote)))
    (rsync--run selected-remote (rsync--get-excludes rsync-excluded-dirs) rsync-local-path dry-run file)))

(provide 'rsync-mode)
;;; rsync-mode.el ends here
