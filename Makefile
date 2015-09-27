EMACS ?= emacs
EFLAGS ?= -Q -L .

ELS = jscs.el
ELCS = $(ELS:.el=.elc)

TEST_EL = test/jscs-test.el

.PHONY: compile
compile: $(ELCS)

%.elc: %.el
	$(EMACS) $(EFLAGS) --batch -f batch-byte-compile $<

.PHONY: clean
clean:
	rm -f $(ELCS)

.PHONY: test
test: compile
	$(EMACS) $(EFLAGS) -l $(TEST_EL) --batch -f ert-run-tests-batch-and-exit

.PHONY: test-deps
test-deps:
	cd test && npm install jscs
