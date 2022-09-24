##
# Cardano.el
#
# @file
# @version 0.1

install-deps:
	./makem.sh --sandbox=testbox --install-deps

all:
	emacs -batch -l runner.el -l ert -l tests/cardano-tests.el -l tests/encodings-test.el -f ert-run-tests-batch-and-exit

# end
