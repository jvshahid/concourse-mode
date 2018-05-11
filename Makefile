.PHONY: test

test:
	emacs --batch -l $(PWD)/../hierarchy/hierarchy.el -l concourse-mode.el -l concourse-tests.el -f ert-run-tests-batch-and-exit
