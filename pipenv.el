;;; pipenv.el --- A Pipenv porcelain inside Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Paul Walsh

;; Author: Paul Walsh <paulywalsh@gmail.com>
;; URL: https://github.com/pwalsh/pipenv.el
;; Version: 0.0.1-alpha
;; Package-Requires: ((emacs "25")(f "0.19.0")(s "1.12.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://github.com/pwalsh/pipenv.el for documentation.

;;; Code:

(require 'f)
(require 'python)
(require 's)

(defgroup pipenv nil
  "A Pipenv porcelain inside Emacs."
  :prefix "pipenv-"
  :group 'python)

(defcustom pipenv-executable
  (executable-find "pipenv")
  "The Pipenv executable."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'pipenv)

(defcustom pipenv-buffer-name
  "*Pipenv*"
  "The name of the Pipenv buffer."
  :type 'string
  :group 'pipenv)

(defcustom pipenv-process-name
  "Pipenv"
  "The name of the Pipenv process."
  :type 'string
  :group 'pipenv)

(defun pipenv--clean-response (response)
  "Clean up string response from shell command."
  (s-chomp response))

(defun pipenv--process-filter-buffer-insert (process response)
  "Process filter to insert process response in process buffer."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html#Filter-Functions
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert response)
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process)))))))

(defun pipenv--process-filter-message-insert (process response)
  "Process filter to generate a message on response."
  (message (concat "Finished " (s-join " " (process-command process)))))

(defun pipenv--process-filter-variable-insert(process response)
  "Process filter to set several global variables after process execution."
  (cond ((and
          (s-equals? (nth 0 (last (process-command process))) "--py")
          (f-file? response))
        (setq python-shell-interpreter response))
        ((and
          (s-equals? (nth 0 (last (process-command process))) "--venv")
          (f-directory? response))
         (setq python-shell-virtualenv-root response)))
  (setq pipenv-process-response response))

(defun pipenv--process-filter (process response)
  (let ((clean-response (pipenv--clean-response response)))
    (pipenv--process-filter-variable-insert process clean-response)
    (pipenv--process-filter-message-insert process clean-response)
    (pipenv--process-filter-buffer-insert process clean-response)))

(defun pipenv--make-pipenv-process (command &optional filter sentinel)
  "Construct a Pipenv process."
  (make-process
   :name pipenv-process-name
   :buffer pipenv-buffer-name
   :command command
   :coding 'utf-8-unix
   :filter filter
   :sentinel sentinel))

(defun pipenv--command (&rest args)
  ""
  (let ((command (cons pipenv-executable args))
        (filter 'pipenv--process-filter))
    (pipenv--make-pipenv-process command filter)))

(defun pipenv-update ()
  "Update Pipenv and pip to latest."
  (interactive)
  (pipenv--command "--update"))

(defun pipenv-where ()
  "Return path to project home directory, or a message if not in a Pipenv project."
  (interactive)
  (pipenv--command "--where"))

(defun pipenv-venv ()
  "Return path to the project venv directory, or a message if not in a Pipenv project."
  (interactive)
  (pipenv--command "--venv"))

(defun pipenv-py ()
  "Return path to project Python, or a message if not in a Pipenv project."
  (interactive)
  (pipenv--command "--py"))

(defun pipenv-python (version)
  "Specify which version of Python virtualenv should use."
  (interactive "sWhich Python version should be used for this project? ")
  (apply 'pipenv--command (list "--python" version)))

(defun pipenv-three ()
  "Use Python 3 when creating virtualenv."
  (interactive)
  (pipenv--command "--three"))

(defun pipenv-two ()
  "Use Python 2 when creating virtualenv."
  (interactive)
  (pipenv--command "--two"))

(defun pipenv-version ()
  "Return the version of the currently installed Pipenv."
  (interactive)
  (pipenv--command "--version"))

(defun pipenv-man ()
  "Return the man page for Pipenv."
  (interactive)
  (pipenv--command "--man"))

(defun pipenv-help ()
  "Return the help for Pipenv."
  (interactive)
  (pipenv--command "--help"))

(defun pipenv-check ()
  "Checks for security vulnerabilities and against PEP 508 \
markers provided in Pipfile."
  (interactive)
  (pipenv--command "check"))

(defun pipenv-graph ()
  "Displays currently-install dependency graph information."
  (interactive)
  (pipenv--command "graph"))

(defun pipenv-install(&rest packages)
  "Installs provided packages and adds them to Pipfile, \
or (if none is given), installs all packages."
  (interactive "sWhich Python packages should be installed? ")
  (apply 'pipenv--command (cons "install" packages)))

(defun pipenv-lock ()
  "Generates Pipfile.lock."
  (interactive)
  (pipenv--command "lock"))

(defun pipenv-open ()
  "View a given module in your editor."
  (interactive)
  (pipenv--command "open"))

(defun pipenv-run (&rest command)
  "Spawns a command installed into the virtualenv."
  (interactive "sEnter the command to call: ")
  (pipenv--command (cons "run" command)))

(defun pipenv-shell ()
  "Spawn a shell within the virtualenv."
  (interactive)
  (let ((name (generate-new-buffer-name (concat "*pipenv shell*"))))
    (pop-to-buffer name)
    (shell (current-buffer))
    (process-send-string nil "pipenv shell\n")
    (comint-clear-buffer)))

(defun pipenv-uninstall(&rest packages)
  "Uninstalls a provided package and removes it from Pipfile."
  (interactive "sWhich Python packages should be uninstalled? ")
  (apply 'pipenv--command (cons "uninstall" packages)))

(defun pipenv-update ()
  "Uninstalls all packages, and reinstalls packages in Pipfile \
to latest compatible versions."
  (interactive)
  (pipenv--command "update"))

(defun pipenv-project? ()
  "Are we in a Pipenv project?"
  (f-traverse-upwards
   (lambda (path) (f-exists? (f-expand "Pipfile" path)))
   "."))

(defalias 'pipenv-project-p 'pipenv-project?)

(defun pipenv-set ()
  "Set the active Python version from Pipenv."
  (interactive)
  (progn
    (pipenv-venv)
    (pipenv-py)))

(defun pipenv-unset ()
  "Unset the active Pipenv version from Pipenv; back to defaults."
  (interactive)
  (setq
   python-shell-virtualenv-root nil
   python-shell-interpreter "python"))

(add-hook 'projectile-after-switch-project-hook #'pipenv-set)

(provide 'pipenv)

;;; pipenv.el ends here
