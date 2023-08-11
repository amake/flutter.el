# Run an arbitrary Emacs version like
#   make test emacs="docker run --rm -it -v $PWD:/work -w /work silex/emacs:26 emacs"
emacs := emacs
elpa_dir := elpa
run_emacs = $(emacs) -Q --batch -L . -L $(elpa_dir) -l package \
	--eval '(setq package-user-dir (expand-file-name "$(elpa_dir)"))' \
	--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	--eval '(package-initialize)'

dependencies := flycheck
dev_dependencies := package-lint
all_deps := $(dependencies) $(dev_dependencies)
test_versions := 25 26 27 28

.PHONY: test
test: ## Compile and run unit tests
test: lint test-compile

define test_one
  .PHONY: test-$(1)
  test-$(1):
	  $$(MAKE) test elpa_dir=elpa-$(1) emacs='docker run --rm -it -v $$(PWD):/work -w /work silex/emacs:$(1) emacs'
endef

$(foreach _,$(test_versions),$(eval $(call test_one,$(_))))

.PHONY: test-matrix
test-matrix: ## Run `test` target on all Emacs versions
test-matrix: $(addprefix test-,$(test_versions))

$(elpa_dir):
	$(run_emacs) \
		--eval '(make-directory "$(@)")' \
		--eval "(let ((to-install (seq-filter (lambda (e) (not (require e nil t))) '($(all_deps))))) \
			(when to-install (package-refresh-contents) (mapc #'package-install to-install)))"

.PHONY: deps
deps: $(elpa_dir)

.PHONY: lint
lint: ## Check for issues
lint: | $(elpa_dir)
	$(run_emacs) \
		-f package-lint-batch-and-exit *.el

.PHONY: test-compile
test-compile: | $(elpa_dir)
	$(run_emacs) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile *.el

.PHONY: prettify
prettify: ## Auto-format code
prettify: el_files := find . -name '*.el' -print0
prettify: | $(elpa_dir)
	$(el_files) | xargs -P 0 -0 -I {} \
		$(run_emacs) \
		$(addprefix -l ,$(dependencies)) \
		{} \
		--eval '(setq indent-tabs-mode nil tab-width 4 require-final-newline t)' \
		--eval '(indent-region (point-min) (point-max))' \
		--eval '(whitespace-cleanup)' \
		--eval '(save-buffer)' \
		> /dev/null

.PHONY: prettify-staged
prettify-staged: staged_el_files := git diff -z --cached --name-only --diff-filter=ACMR | grep -z '\.el'
prettify-staged:
	modified=$$($(staged_el_files) | xargs -0); \
	if [ -n "$$modified" ]; then \
		for file in $$modified; do git show ":$$file" >"$$file.tmp.el"; done; \
		$(MAKE) prettify el_files="($(staged_el_files); find . -name '*.tmp.el' -print0)"; \
		for file in $$modified; do \
			hash=$$(git hash-object -w "$$file.tmp.el"); \
			git update-index --add --cacheinfo 100644 "$$hash" "$$file"; \
		done; \
		find . -name '*.tmp.el' -delete; \
		if [ -z "$$(git diff --cached --name-only)" ]; then echo "No files left after formatting" 1>&2; exit 1; fi \
	fi

.PHONY: clean
clean: ## Clean files
	rm -f *.elc

.PHONY: clobber
clobber: ## Remove all generated files
clobber: clean
	rm -rf $(elpa_dir)*

# Hooks

hooks := $(filter-out %~,$(wildcard hooks/*))
git_dir := $(shell git rev-parse --git-dir)

.PHONY: hooks
hooks: ## Install helpful git hooks
hooks: $(addprefix $(git_dir)/,$(hooks))

$(git_dir)/hooks/%: hooks/%
	ln -s $(PWD)/$(<) $(@)

.PHONY: help
help: ## Show this help text
	$(info usage: make [target])
	$(info )
	$(info Available targets:)
	@awk -F ':.*?## *' '/^[^\t].+?:.*?##/ \
         {printf "  %-24s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
