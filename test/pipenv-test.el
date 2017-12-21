;; Test suite for pipenv.el

(require 'ert-async)
(require 'f)
(require 's)

(load (f-expand "pipenv.el" default-directory))

(setq existing-project (f-expand "existing-project" default-directory))
(setq new-project (f-expand "new-project" temporary-file-directory))
(setq pipenv-accept-timeout nil)

(ert-deftest-async pipenv-with-no-active-project (done)
  "Basic behavior when we do not have an active project."
  (accept-process-output (pipenv-where) pipenv-accept-timeout)
  (should (not (f-directory? pipenv-process-response)))

  (accept-process-output (pipenv-venv) pipenv-accept-timeout)
  (should (not (f-directory? pipenv-process-response)))

  (accept-process-output (pipenv-py) pipenv-accept-timeout)
  (should (not (f-directory? pipenv-process-response)))

  (should (s-equals? nil python-shell-virtualenv-path))
  (should (s-equals? nil python-shell-virtual\env-root))
  (should (s-equals? "python" python-shell-interpreter))
  (funcall done))

(ert-deftest-async pipenv-with-existing-project (done)
  "Basic behavior when we are in an existing project."
  (cd existing-project)

  (accept-process-output (pipenv-set) pipenv-accept-timeout)

  (accept-process-output (pipenv-where) pipenv-accept-timeout)
  (should (s-ends-with? (f-filename existing-project) pipenv-process-response))

  (accept-process-output (pipenv-venv) pipenv-accept-timeout)
  (should (s-contains? (f-filename existing-project) pipenv-process-response))

  (accept-process-output (pipenv-py) pipenv-accept-timeout)
  (should (s-contains? (f-filename existing-project) pipenv-process-response))

  (should (s-contains? (f-filename existing-project) python-shell-virtualenv-path))
  (should (s-contains? (f-filename existing-project) python-shell-virtualenv-root))
  (should (s-contains? (f-filename existing-project) python-shell-interpreter))

  (pipenv-unset)

  (should (s-equals? nil python-shell-virtualenv-path))
  (should (s-equals? nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter))
  (funcall done))

(ert-deftest-async pipenv-with-new-project (done)
  "Basic behavior when we create and enter a new project."
  (f-mkdir new-project)
  (cd new-project)
  (accept-process-output (pipenv-python "2") pipenv-accept-timeout)
  (accept-process-output (pipenv-set) pipenv-accept-timeout)

  (accept-process-output (pipenv-where) pipenv-accept-timeout)
  (should (s-ends-with? (f-filename new-project) pipenv-process-response))

  ;;(accept-process-output (pipenv-venv) pipenv-accept-timeout)
  ;;(should (s-contains? (f-filename new-project) pipenv-process-response))

  (accept-process-output (pipenv-py) pipenv-accept-timeout)
  (should (s-contains? (f-filename new-project) pipenv-process-response))

  ;;(should (s-contains? (f-filename new-project) python-shell-virtualenv-path))
  ;;(should (s-contains? (f-filename new-project) python-shell-virtualenv-root))
  (should (s-contains? (f-filename new-project) python-shell-interpreter))

  (pipenv-unset)

  (should (s-equals? nil python-shell-virtualenv-path))
  (should (s-equals? nil python-shell-virtualenv-root))
  (should (s-equals? "python" python-shell-interpreter))

  (f-delete new-project 1)
  (funcall done))
