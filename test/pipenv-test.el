;; Test suite for pipenv.el

(require 'ert)
(require 'f)
(require 's)
(require 'flycheck)

(load (f-expand "pipenv.el" default-directory))

(setq existing-project (f-expand "existing-project" default-directory))
(setq new-project (f-expand "new-project" temporary-file-directory))

(ert-deftest test-pipenv--clean-response ()
  (let ((response "example/response\n")
        (expected "example/response"))
    (should (s-equals? (pipenv--clean-response response) expected))))

(ert-deftest test-pipenv--force-list ()
  (should (equal (pipenv--force-list "this that") '("this" "that")))
  (should (equal (pipenv--force-list "this     that") '("this" "that")))
  (should (equal (pipenv--force-list '("this" "that")) '("this" "that"))))

(ert-deftest test-pipenv--get-executables-dir ()
  (let ((python-shell-virtualenv-root nil))
    (should (eq (pipenv--get-executables-dir) nil)))
  (let ((python-shell-virtualenv-root "/directory/that/does/not/exist"))
    (should (eq (pipenv--get-executables-dir) nil)))
  (let ((python-shell-virtualenv-root default-directory)) ;;just tests logic
    (should (equal (pipenv--get-executables-dir)
                   (concat default-directory "bin")))))

(ert-deftest pipenv-with-no-active-project ()
  "Basic behavior when we do not have an active project."
  (should (not (pipenv-project?)))

  (pipenv--force-wait (pipenv-where))
  (should (s-contains? "No Pipfile present at project home" pipenv-process-response))

  (pipenv--force-wait (pipenv-venv))
  (should (s-contains? "Aborted!" pipenv-process-response))

  (pipenv--force-wait (pipenv-py))
  (should (s-contains? "location not created nor specified" pipenv-process-response))

  (should (eq nil python-shell-virtualenv-path))
  (should (eq nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter)))

(defun python-from-venv? (folder)
  (setq which-python (shell-command-to-string "which python"))
  (s-contains? (f-filename folder) which-python))

(ert-deftest pipenv-behavior-with-new-project ()

  (f-mkdir new-project)
  (cd new-project)

  (should (not (pipenv-project?)))

  (pipenv--force-wait (pipenv-python "2"))

  (should (pipenv-project?))
  (should-not (python-from-venv? new-project))

  (pipenv-activate)
  (should (python-from-venv? new-project))

  (pipenv--force-wait (pipenv-where))
  (should (s-ends-with? (f-filename new-project) pipenv-process-response))

  (pipenv--force-wait (pipenv-venv))
  (should (s-contains? (f-filename new-project) pipenv-process-response))

  (should (s-contains? (f-filename new-project) python-shell-virtualenv-path))
  (should (s-contains? (f-filename new-project) python-shell-virtualenv-root))

  (let ((venv-executables (pipenv--get-executables-dir)))
    (should (member venv-executables exec-path))
    (should (not (executable-find "ipython")))
    (pipenv--force-wait (pipenv-install "ipython"))
    (should (executable-find "ipython"))
    (should (s-contains? venv-executables (executable-find "ipython")))
    (should (s-contains? "ipython" (f-read-text "Pipfile" 'utf-8)))
    (pipenv--force-wait (pipenv-uninstall "ipython"))
    (should (not (executable-find "ipython")))
    (should (not (s-contains? "ipython" (f-read-text "Pipfile" 'utf-8)))))

  (pipenv-deactivate)
  (should-not (python-from-venv? new-project))

  (should (eq nil python-shell-virtualenv-path))
  (should (eq nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter))
  (let ((venv-executables (pipenv--get-executables-dir)))
    (should (not (member venv-executables exec-path))))

  (f-delete new-project 1))

(ert-deftest pipenv-behavior-with-existing-project ()

  (cd existing-project)

  (should (pipenv-project?))
  (should-not (python-from-venv? existing-project))
  
  (pipenv-activate)
  (should (python-from-venv? existing-project))

  (pipenv--force-wait (pipenv-where))
  (should (s-ends-with? (f-filename existing-project) pipenv-process-response))

  (pipenv--force-wait (pipenv-venv))
  (should (s-contains? (f-filename existing-project) pipenv-process-response))

  (should (s-contains? (f-filename existing-project) python-shell-virtualenv-path))
  (should (s-contains? (f-filename existing-project) python-shell-virtualenv-root))

  (let ((venv-executables (pipenv--get-executables-dir)))
    (should (member venv-executables exec-path))
    (should (not (executable-find "ipython")))
    (pipenv--force-wait (pipenv-install "ipython"))
    (should (executable-find "ipython"))
    (should (s-contains? venv-executables (executable-find "ipython")))
    (should (s-contains? "ipython" (f-read-text "Pipfile" 'utf-8)))
    (pipenv--force-wait (pipenv-uninstall "ipython"))
    (should (not (executable-find "ipython")))
    (should (not (s-contains? "ipython" (f-read-text "Pipfile" 'utf-8)))))

  (pipenv-deactivate)
  (should-not (python-from-venv? existing-project))

  (should (eq nil python-shell-virtualenv-path))
  (should (eq nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter))
  (let ((venv-executables (pipenv--get-executables-dir)))
    (should (not (member venv-executables exec-path)))))

(ert-deftest flycheck-integration ()
  (cd existing-project)
  (pipenv--force-wait (pipenv-install "flake8"))
  (find-file "some.py")
  (flycheck-mode)
  (should-not (flycheck-may-use-checker 'python-flake8))
  (pipenv-activate)
  (should-not (flycheck-disabled-checker-p 'python-flake8))
  (pipenv-deactivate)
  (should (flycheck-disabled-checker-p 'python-flake8))
  (pipenv--force-wait (pipenv-uninstall "flake8"))
)
