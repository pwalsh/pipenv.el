;;; pipenv.el --- A Pipenv porcelain  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 by Paul Walsh

;; Author: Paul Walsh <paulywalsh@gmail.com>
;; URL: https://github.com/pwalsh/pipenv.el
;; Version: 0.0.1-beta
;; Package-Requires: ((emacs "25.1") (s "1.12.0") (pyvenv "1.20") (load-env-vars "0.0.2"))

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

(require 'python)
(require 's)
(require 'subr-x)
(require 'pyvenv)

(defgroup pipenv nil
  "A Pipenv porcelain."
  :prefix "pipenv-"
  :group 'python)

;;
;; User-facing customization.
;;

(defcustom pipenv-executable
  "pipenv"
  "The Pipenv executable."
  :type '(file :must-match t)
  :safe #'file-directory-p
  :group 'pipenv)

(defcustom pipenv-process-name
  "Pipenv"
  "The name of the Pipenv process."
  :type 'string
  :group 'pipenv)

(defcustom pipenv-process-buffer-name
  "*Pipenv*"
  "The name of the Pipenv process buffer."
  :type 'string
  :group 'pipenv)

(defcustom pipenv-shell-buffer-name
  "*Pipenv shell*"
  "The name of the Pipenv shell buffer."
  :type 'string
  :group 'pipenv)

(defcustom pipenv-shell-buffer-init-command
  "exec pipenv shell"
  "The shell command to initialize the Pipenv shell."
  :type 'string
  :group 'pipenv)

(defcustom pipenv-with-flycheck
  t
  "Use the Pipenv virtual environment when searching for Flycheck executables."
  :type 'boolean
  :group 'pipenv)

(defcustom pipenv-with-projectile
  t
  "Provide hooks for Projectile when a Pipenv project is detected."
  :type 'boolean
  :group 'pipenv)

(defcustom pipenv-projectile-after-switch-function
  #'pipenv-projectile-after-switch-default
  "The function to add to projectile-after-switch-hook."
  :type 'function
  :group 'pipenv)

(defcustom pipenv-keymap-prefix (kbd "C-c C-p")
  "Pipenv keymap prefix."
  :group 'pipenv
  :type 'string)

;;
;; Helper functions internal to the package.
;;

(defun pipenv--initialize ()
  "Initialization steps to run when the pipenv package is required."
  (when pipenv-with-projectile
    (pipenv-activate-projectile)))

(defun pipenv--clean-response (response)
  "Clean up RESPONSE from shell command."
  (car
   (s-lines response)))

(defun pipenv--force-list (argument)
  "Force ARGUMENT to a list."
  (if (listp argument)
      argument
    (s-split " " argument t)))

(defun pipenv--force-wait (process)
  "Block until PROCESS exits successfully."
  ;; Adding a sit-for in a sentinel seems to give the process time to update its
  ;; status which can help process-live-p from ever returning true
  (set-process-sentinel process (lambda (proc event) (sit-for 0.1)))
  (while (process-live-p process)
    (sit-for 0.1 t)))

(defun pipenv--process-filter-buffer-insert (process response)
  "Filter for PROCESS, insert RESPONSE in process buffer."
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Filter-Functions.html#Filter-Functions
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark process))
          (insert response)
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process))))
      ;; Interpret ANSI escape sequences from Pipenv
      (ansi-color-apply-on-region (point-min) (point-max)))))

(defun pipenv--process-filter-variable-insert (process response)
  "Filter for PROCESS, which sets several global variables based on RESPONSE."
  (when (and
         (s-equals? (nth 0 (last (process-command process))) "--venv")
         (file-directory-p response))
    (setq python-shell-virtualenv-root response)))

(defun pipenv--process-filter (process response)
  "Pipenv default filter stack PROCESS and RESPONSE handling."
  (let ((clean-response (pipenv--clean-response response)))
    (pipenv--process-filter-variable-insert process clean-response)
    (pipenv--process-filter-buffer-insert process clean-response)))

(defun pipenv--get-executables-dir ()
  "Get the directory of executables in an active virtual environment, or nil."
  (when (and python-shell-virtualenv-root
             (file-directory-p python-shell-virtualenv-root))
    (concat
     (file-name-as-directory python-shell-virtualenv-root)
     (if (eq system-type 'windows-nt) "Scripts" "bin"))))

(defun pipenv--make-pipenv-process (command &optional filter sentinel)
  "Make a Pipenv process from COMMAND; optional custom FILTER or SENTINEL."
  (make-process
   :name pipenv-process-name
   :buffer pipenv-process-buffer-name
   :command command
   :coding 'utf-8-unix
   :filter filter
   :sentinel sentinel
   :connection-type 'pipe))

(defun pipenv--command (args)
  "Call Pipenv with ARGS and the default filter stack."
  (let ((command (cons pipenv-executable args))
        (filter 'pipenv--process-filter)
        (sentinel 'pipenv--messaging-sentinel))
    (pipenv--make-pipenv-process command filter sentinel)))

(defun pipenv--f-parent (path)
  "Return the parent directory to PATH.  see `f-parent'."
  (let ((parent (file-name-directory
                 (directory-file-name (f-expand path default-directory)))))
    (if (file-name-absolute-p path)
        (directory-file-name parent)
      (file-relative-name parent))))

;;
;; Interactive commands that implement the Pipenv interface in Emacs.
;;

(defun pipenv-where ()
  "Return path to project home directory, or a message if not in a Pipenv project."
  (interactive)
  (pipenv--command (list "--where")))

(defun pipenv-venv ()
  "Return path to the project venv directory, or a message if not in a Pipenv project."
  (interactive)
  (pipenv--command (list "--venv")))

(defun pipenv-py ()
  "Return path to project Python, or a message if not in a Pipenv project."
  (interactive)
  (pipenv--command (list "--py")))

(defun pipenv-envs ()
  "Return Pipenv's environment variable options."
  (interactive)
  (pipenv--command (list "--envs")))

(defun pipenv-rm ()
  "Remove the virtualenv of the current project."
  (interactive)
  (pipenv--command (list "--rm")))

(defun pipenv-completion ()
  "Return output completion for eval in a shell."
  (interactive)
  (pipenv--command (list "--completion")))

(defun pipenv-man ()
  "Return the man page for Pipenv."
  (interactive)
  (pipenv--command (list "--man")))

(defun pipenv-three ()
  "Use Python 3 when creating virtualenv."
  (interactive)
  (pipenv--command (list "--three")))

(defun pipenv-two ()
  "Use Python 2 when creating virtualenv."
  (interactive)
  (pipenv--command (list "--two")))

(defun pipenv-python (version)
  "Specify which VERSION of Python virtualenv should use."
  (interactive "sWhich Python version should be used for this project? ")
  (pipenv--command (list "--python" version)))

(defun pipenv-version ()
  "Return the version of the currently installed Pipenv."
  (interactive)
  (pipenv--command (list "--version")))

(defun pipenv-help ()
  "Return the help for Pipenv."
  (interactive)
  (pipenv--command (list "--help")))

(defun pipenv-check ()
  "Check for security vulnerabilities and against PEP 508 \
markers provided in Pipfile."
  (interactive)
  (pipenv--command (list "check")))

(defun pipenv-graph ()
  "Displays currently-install dependency graph information."
  (interactive)
  (pipenv--command (list "graph")))

(defun pipenv-install(packages)
  "Installs PACKAGES and adds them to Pipfile,
or (if none is given), installs all packages."
  (interactive "sWhich Python packages should be installed (separate with space)? ")
  (pipenv--command (cons "install" (pipenv--force-list packages))))

(defun pipenv-lock ()
  "Generate Pipfile.lock."
  (interactive)
  (pipenv--command (list "lock")))

(defun pipenv-open (module)
  "View a given MODULE in your editor."
  (interactive "sWhich Python module do you want to view? ")
  (let* ((template "import %s as mod; print(mod.__file__)")
         (replacements '(("pyo" . "py") ("pyc" . "py") ("pyd" . "py")))
         (suffix "__init__.py")
         (response (pipenv--check-output
                    python-shell-interpreter "-c" (format template module)))
         (real-path (s-with response
                      (s-chomp)
                      (s-trim)
                      (s-replace-all replacements)))
         (ideal-path (if (s-contains? suffix real-path)
                         (pipenv--f-parent real-path)
                       real-path)))
    (find-file ideal-path)))

(defun pipenv--messaging-sentinel (process event)
  "Send EVENT notifications for PROCESS to *Messages* buffer and to process buffer."
  (let ((msg (format "%s %s" (s-join " " (process-command process)) (s-chomp event))))
    (message msg)
    (with-current-buffer (process-buffer process)
      (insert "\n")
      (insert msg))))

(defun pipenv--check-output (&rest command)
  "Run COMMAND and return its standard output.

A poor-man's equivalent of subprocess.check_output in Python."
  (with-temp-buffer
    (let* ((pipenv-process-name (concat "pipenv-check-output-" (car command)))
           (pipenv-process-buffer-name (buffer-name))
           (proc (pipenv--make-pipenv-process command nil 'pipenv--messaging-sentinel)))
      (pipenv--force-wait proc)
      (s-trim (buffer-string)))))

(defun pipenv-run (command)
  "Spawns a COMMAND installed into the virtualenv."
  (interactive "sEnter the command to call: ")
  (pipenv--command (cons "run" (pipenv--force-list command))))

(defun pipenv-shell ()
  "Spawn a shell within the virtualenv."
  (interactive)
  (let ((name (generate-new-buffer-name pipenv-shell-buffer-name)))
    (pop-to-buffer name)
    (shell (current-buffer))
    (insert pipenv-shell-buffer-init-command)
    (setq-local comint-process-echoes t)
    (comint-send-input)
    (comint-clear-buffer)))

(defun pipenv-uninstall (packages)
  "Uninstall PACKAGES and remove from Pipfile."
  (interactive "sWhich Python packages should be uninstalled (separate with space)? ")
  (pipenv--command (cons "uninstall" (pipenv--force-list packages))))

(defun pipenv-update ()
  "Uninstalls all packages, and reinstalls packages in Pipfile \
to latest compatible versions."
  (interactive)
  (pipenv--command (list "update")))

;;
;; High-level interactive commands enabled by the Pipenv interface.
;;


(defun pipenv-activate ()
  "Activate the Python version from Pipenv.  Return nil if no project."
  (interactive)
  (when-let ((root (pipenv-project?)))
    (pipenv--force-wait (pipenv-venv))
    (pyvenv-activate (directory-file-name python-shell-virtualenv-root))
    (let ((env-file (expand-file-name ".env" root)))
      (when (file-exists-p env-file)
        (load-env-vars env-file)))
    (when (and (featurep 'flycheck) pipenv-with-flycheck)
      (pipenv-activate-flycheck))
    t))

(defun pipenv-deactivate ()
  "Deactivate the Python version from Pipenv; back to defaults."
  (interactive)
  (pyvenv-deactivate)
  (setq python-shell-virtualenv-root nil)
  (when (and (featurep 'flycheck) pipenv-with-flycheck)
    (pipenv-deactivate-flycheck))
  t)

;;
;; User-facing utility functions.
;;

(defun pipenv-project? ()
  "Are we in a Pipenv project?"
  (locate-dominating-file default-directory "Pipfile"))

(defalias 'pipenv-project-p 'pipenv-project?)

(defun pipenv-installed? ()
  "Can the Pipenv executable found?"
  pipenv-executable)

(defalias 'pipenv-installed-p 'pipenv-installed?)

(defun pipenv-executable-find (executable)
  "Find EXECUTABLE in the executable path of an activate virtual environment."
  (when (bound-and-true-p python-shell-virtualenv-root)
    (let ((exec-path (python-shell-calculate-exec-path)))
      (executable-find executable))))

;;
;; Integration with 3rd party packages.
;;

(defvar flycheck-disabled-checkers)
(defvar flycheck-enabled-checkers)
(defvar flycheck-executable-find)

(defun pipenv--verify-python-checkers ()
  "Manually verify checkers for `python-mode'."
  (let ((checkers (flycheck-defined-checkers 'modes)))
    (while checkers
      (let ((checker (car checkers)))
        (when (memq 'python-mode (flycheck-checker-get checker 'modes))
          (setq flycheck-disabled-checkers (remq checker flycheck-disabled-checkers))
          (setq flycheck-enabled-checkers (remq checker flycheck-enabled-checkers))
          (flycheck-may-use-checker checker)))
      (setq checkers (cdr checkers)))))

(defun pipenv-activate-flycheck ()
  "Activate integration of Pipenv with Flycheck."
  (setq flycheck-executable-find #'pipenv-executable-find)
  (pipenv--verify-python-checkers))

(defun pipenv-deactivate-flycheck ()
  "Deactivate integration of Pipenv with Flycheck."
  (setq flycheck-executable-find #'flycheck-default-executable-find)
  (pipenv--verify-python-checkers))

(defun pipenv-activate-projectile ()
  "Activate integration of Pipenv with Projectile."
  (add-hook
   'projectile-after-switch-project-hook
   (lambda () (funcall pipenv-projectile-after-switch-function))))

(defun pipenv-projectile-after-switch-default ()
  "When a Pipenv project is found, activate the virtual environment."
  ;; Always clean up, in case we were in a Python project previously.
  (pipenv-deactivate)
  ;; Only activate if we can verify this is a Pipenv project.
  (when (pipenv-project?)
    (pipenv-activate)))

(defun pipenv-projectile-after-switch-extended ()
  "When a Pipenv project is found, activate the virtual environment, \
and open a Pipenv shell and a Python interpreter."
  ;; Always clean up, in case we were in a Python project previously.
  (pipenv-deactivate)
  ;; Only activate if we can verify this is a Pipenv project.
  (when (pipenv-project?)
    (pipenv-activate)
    (pipenv-shell)
    (run-python)))

;;
;; Initialization code.
;;

(pipenv--initialize)

;;
;; Core Emacs integration.
;;

(defvar pipenv-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'pipenv-activate)
    (define-key map (kbd "d") 'pipenv-deactivate)
    (define-key map (kbd "s") 'pipenv-shell)
    (define-key map (kbd "o") 'pipenv-open)
    (define-key map (kbd "i") 'pipenv-install)
    (define-key map (kbd "u") 'pipenv-uninstall)
    map))

(defvar pipenv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map pipenv-keymap-prefix pipenv-command-map)
    map)
  "Keymap for pipenv mode.")

;;;###autoload
(define-minor-mode pipenv-mode
  "Minor mode for Pipenv."
  :lighter " Pipenv"
  :keymap pipenv-mode-map)

(provide 'pipenv)

;;; pipenv.el ends here
