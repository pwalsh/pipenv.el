;; Test suite for pipenv.el

(require 'ert)
(require 'f)
(require 's)

(load (f-expand "pipenv.el" default-directory))

(setq existing-project (f-expand "existing-project" default-directory))
(setq new-project (f-expand "new-project" temporary-file-directory))
(setq pipenv-accept-timeout nil)

(ert-deftest pipenv-with-no-active-project ()
  "Basic behavior when we do not have an active project."

  (accept-process-output (pipenv-where) pipenv-accept-timeout)
  (should (not (f-directory? pipenv-process-response)))

  (accept-process-output (pipenv-venv) pipenv-accept-timeout)
  (should (not (f-directory? pipenv-process-response)))

  (accept-process-output (pipenv-py) pipenv-accept-timeout)
  (should (not (f-directory? pipenv-process-response)))

  (should (s-equals? nil python-shell-virtualenv-path))
  (should (s-equals? nil python-shell-virtual\env-root))
  (should (s-equals? "python" python-shell-interpreter)))

(ert-deftest pipenv-with-existing-project ()
  "Basic behavior when we are in an existing project."

  (cd existing-project)

  (accept-process-output (pipenv-activate) pipenv-accept-timeout)

  (accept-process-output (pipenv-where) pipenv-accept-timeout)
  (should (s-ends-with? (f-filename existing-project) pipenv-process-response))

  (accept-process-output (pipenv-venv) pipenv-accept-timeout)
  (should (s-contains? (f-filename existing-project) pipenv-process-response))

  (accept-process-output (pipenv-py) pipenv-accept-timeout)
  (should (s-contains? (f-filename existing-project) pipenv-process-response))

  (should (s-contains? (f-filename existing-project) python-shell-virtualenv-path))
  (should (s-contains? (f-filename existing-project) python-shell-virtualenv-root))
  (should (s-contains? (f-filename existing-project) python-shell-interpreter))

  (pipenv-deactivate)

  (should (s-equals? nil python-shell-virtualenv-path))
  (should (s-equals? nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter)))

(ert-deftest pipenv-with-new-project ()
  "Basic behavior when we create and enter a new project."

  (f-mkdir new-project)
  (cd new-project)

  (accept-process-output (pipenv-python "2") pipenv-accept-timeout)

  ;; Creating a project takes a while, in a subprocess of our shell call,
  ;; so we need to sleep for long enough to let it finish.
  (sleep-for 3)

  (accept-process-output (pipenv-activate) pipenv-accept-timeout)

  (accept-process-output (pipenv-where) pipenv-accept-timeout)
  (princ pipenv-process-response)
  (should (s-ends-with? (f-filename new-project) pipenv-process-response))

  (accept-process-output (pipenv-venv) pipenv-accept-timeout)
  (should (s-contains? (f-filename new-project) pipenv-process-response))

  (accept-process-output (pipenv-py) pipenv-accept-timeout)
  (should (s-contains? (f-filename new-project) pipenv-process-response))

  (should (s-contains? (f-filename new-project) python-shell-virtualenv-path))
  (should (s-contains? (f-filename new-project) python-shell-virtualenv-root))
  (should (s-contains? (f-filename new-project) python-shell-interpreter))

  (pipenv-deactivate)

  (should (s-equals? nil python-shell-virtualenv-path))
  (should (s-equals? nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter))

  (f-delete new-project 1))
