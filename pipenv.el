;;; pipenv.el --- Work with Pipenv from Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Paul Walsh

;; Author: Paul Walsh <paulywalsh@gmail.com>
;; URL: https://github.com/pwalsh/pipenv.el
;; Version: 0.0.1-alpha
;; Package-Requires: ((emacs "24")(s "1.12.0")(f "0.19.0"))

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
  "Pipenv integration with Emacs."
  :prefix "pipenv-"
  :group 'languages)

(defcustom pipenv-executable
  (executable-find "pipenv")
  "The Pipenv executable.."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'pipenv)

(defun pipenv--build-command (command-args)
  "Build shell command as a string"
  (s-join " " command-args))

(defun pipenv--clean-response (response)
  "Clean up string response from shell command."
  (s-chomp response))

(defun pipenv--command (&rest args)
  "command as string helper. args is a list of arguments to the command."
  (let* ((command-args (cons pipenv-executable args))
         (command (pipenv--build-command command-args)))
    (pipenv--clean-response (shell-command-to-string command))))

(defun pipenv-update ()
  "Update Pipenv and pip to latest."
  (interactive)
  (pipenv--command "--update"))

(defun pipenv-where ()
  "Return path to project home directory, or nil if not in a Pipenv project."
  (interactive)
  (let* ((response (pipenv--command "--where")))
    (if (f-directory? response)
        response
      nil)))

(defun pipenv-venv ()
  "Return path to the project venv directory, or nil if not in a Pipenv project."
  (interactive)
  (let* ((response (pipenv--command "--venv")))
    (if (f-directory? response)
        response
      nil)))

(defun pipenv-py ()
  "Return path to project Python, or nil if not in a Pipenv project."
  (interactive)
  (let* ((response (pipenv--command "--py")))
    (if (f-file? response)
        response
      nil)))

(defun pipenv-man ()
  "Return the man page for Pipenv."
  (interactive)
  (princ (pipenv--command "--man")))

(defun pipenv-python (version)
  "Specify which version of Python virtualenv should use."
  (apply 'pipenv--command (list "--python" version)))

(defun pipenv-version ()
  "Return the version of the currently installed Pipenv."
  (interactive)
  (pipenv--command "--version"))

(defun pipenv-help ()
  "Return the help for Pipenv."
  (interactive)
  (princ (pipenv--command "--help")))

(defun pipenv-check ()
  "Checks for security vulnerabilities and against PEP 508 \
markers provided in Pipfile."
  (pipenv--command "check"))

(defun pipenv-graph ()
  "Displays currently-install dependency graph information."
  (pipenv--command "graph"))

(defun pipenv-install(&rest packages)
  "Installs provided packages and adds them to Pipfile, \
or (if none is given), installs all packages."
  (apply 'pipenv--command (cons "install" packages)))

(defun pipenv-lock ()
  "Generates Pipfile.lock."
  (pipenv--command "lock"))

(defun pipenv-uninstall(&rest packages)
  "Uninstalls a provided package and removes it from Pipfile."
  (apply 'pipenv--command (cons "uninstall" packages)))

(defun pipenv-update ()
  "Uninstalls all packages, and reinstalls packages in Pipfile \
to latest compatible versions."
  (pipenv--command "update"))

(defun pipenv-shell ()
  "Spawn a shell within the virtualenv."
  (interactive)
  (let ((name (generate-new-buffer-name (concat "*shell " (pipenv-where) "*"))))
    (pop-to-buffer name)
    (shell (current-buffer))
    (process-send-string nil "pipenv shell\n")))

(defun pipenv-project? ()
  "Are we in a Pipenv project?"
  (f-traverse-upwards
   (lambda (path) (f-exists? (f-expand "Pipfile" path)))
   "."))

(defalias 'pipenv-project-p 'pipenv-project?)

(defun pipenv-set ()
  "Set the active Python version from Pipenv."
  (setq
   python-shell-virtualenv-root (pipenv-venv)
   python-shell-interpreter (or (pipenv-py) "python")))

(defun pipenv-unset ()
  "Unset the active Pipenv version from Pipenv; back to defaults."
  (setq
   python-shell-virtualenv-root nil
   python-shell-interpreter "python"))

(provide 'pipenv)

;;; pipenv.el ends here
