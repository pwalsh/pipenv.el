.PHONY: help install test
.DEFAULT_GOAL := help

help: # http://marmelab.com/blog/2016/02/29/auto-documented-makefile.html
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: ## install dependencies for tests
	cask install --dev

lint: ## run linter
	cask exec emacs -Q -batch -l test/lint-init.el -l package-lint.el -f package-lint-batch-and-exit pipenv.el

test: ## run tests
	cask exec ert-runner
