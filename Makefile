EMACS := emacs
EMACS_VER = docker run --rm -t \
	-v $(PWD):/work \
	-w /work \
	flycheck/emacs-cask:$1 \
	emacs
COMPILE_CMD = $(EMACS) -Q -L . \
	--eval '(setq byte-compile-error-on-warn t)' \
	-batch -f batch-byte-compile
EL_FILES := $(wildcard *.el)

.PHONY: test test-default test-25 test-24
test: test-default test-25 test-24

.PHONY: test-default
test-default test-25 test-24: $(EL_FILES)
	$(COMPILE_CMD) $(^)

test-25: EMACS := $(call EMACS_VER,25.3)

test-24: EMACS := $(call EMACS_VER,24.5)
