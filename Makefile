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

.PHONY: test
test: test-default

.PHONY: test-ci
test-ci: test-26 test-25 test-24

.PHONY: test-default test-26 test-25 test-24
test-default test-26 test-25 test-24: $(EL_FILES)
	$(COMPILE_CMD) $(^)

test-26: EMACS := $(call EMACS_VER,26.2)

test-25: EMACS := $(call EMACS_VER,25.3)

test-24: EMACS := $(call EMACS_VER,24.5)

# Hooks

HOOKS := $(filter-out %~,$(wildcard hooks/*))
GIT_DIR := $(shell git rev-parse --git-dir)

.PHONY: hooks
hooks: $(foreach _,$(HOOKS),$(GIT_DIR)/hooks/$(notdir $(_)))

$(GIT_DIR)/hooks/%: hooks/%
	ln -s $(PWD)/$(<) $(@)
