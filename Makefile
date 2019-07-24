EMACS := emacs
EMACS_VER = docker run --rm -t \
	-v $(PWD):/work \
	-w /work \
	flycheck/emacs-cask:$1 \
	emacs
DEPENDENCIES :=
FIND_PKG_DIR = $(shell find -L ~/.emacs.d/elpa -type d -regex '.*/$1-[0-9.]*')
SEARCH_DIRS = $(foreach _,$(DEPENDENCIES),-L $(call FIND_PKG_DIR,$(_)))
COMPILE_CMD = $(EMACS) -Q -L . $(SEARCH_DIRS) \
	--eval '(setq byte-compile-error-on-warn t)' \
	-batch -f batch-byte-compile
EL_FILES := $(wildcard *.el)

.PHONY: test
test: test-default ## Run regular test (default Emacs)

.PHONY: test-ci
test-ci: test-26 test-25 test-24 ## Run tests for CI (various containerized Emacsen)

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
hooks: ## Install helpful git hooks
hooks: $(foreach _,$(HOOKS),$(GIT_DIR)/hooks/$(notdir $(_)))

$(GIT_DIR)/hooks/%: hooks/%
	ln -s $(PWD)/$(<) $(@)

.PHONY: help
help: ## Show this help text
	$(info usage: make [target])
	$(info )
	$(info Available targets:)
	@awk -F ':.*?## *' '/^[^\t].+?:.*?##/ \
         {printf "  %-24s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
