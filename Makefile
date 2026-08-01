.PHONY: all build test test-small-stack check-duplication check-duplication-gate check-doc-links check-rejection-gate check-root-cleanliness clean help libfortfront.a

# Default target
all: build

# Build the project (convenience wrapper)
build:
	fpm build

libfortfront.a:
	@echo "Building fortfront static library..."
	fpm build
	@lib_path=$$(find build -type f -path 'build/gfortran_*/fortfront/libfortfront.a' 2>/dev/null | xargs ls -t 2>/dev/null | head -n 1); \
	if [ -z "$$lib_path" ]; then \
	    echo "ERROR: no libfortfront.a produced by fpm build"; \
	    exit 1; \
	fi; \
	lib_dir=$$(dirname "$$lib_path"); \
	echo "Found library build directory: $$lib_dir"; \
	build_root=$$(dirname "$$lib_dir"); \
	mod_dir=""; \
	if ls "$$build_root"/*.mod >/dev/null 2>&1; then \
	    mod_dir="$$build_root"; \
	else \
	    mod_dir=$$(for d in $$(ls -dt build/gfortran_* 2>/dev/null); do \
	        if ls "$$d"/*.mod >/dev/null 2>&1; then echo "$$d"; break; fi; \
	    done); \
	fi; \
	if [ -z "$$mod_dir" ]; then \
	    echo "ERROR: no module files produced by fpm build"; \
	    exit 1; \
	fi; \
	echo "Found module build directory: $$mod_dir"; \
	rm -rf build/fortfront_modules; \
	mkdir -p build/fortfront_modules; \
	find "$$mod_dir" -maxdepth 1 -name '*.mod' -exec cp {} build/fortfront_modules/ \; ; \
	if ! ls build/fortfront_modules/*.mod >/dev/null 2>&1; then \
	    echo "ERROR: no module files copied into build/fortfront_modules"; \
	    exit 1; \
	fi; \
	rm -f libfortfront.a; \
	cp "$$lib_path" libfortfront.a


# Run tests (convenience wrapper)
test:
	fpm test

# Run tests with a small stack limit (simulate Windows)
# Override with: make test-small-stack TEST_STACK_KB=1536
TEST_STACK_KB ?= 1024
test-small-stack:
	ulimit -s $(TEST_STACK_KB); fpm test

# Check for test duplication violations (CLAUDE.md compliance)
check-duplication:
	@python3 scripts/check_test_duplication.py

# Negative control (issue #2910): prove the duplication gate can still fail.
check-duplication-gate:
	@bash scripts/test_check_test_duplication.sh

# Corpus rejection gate (issue #2924): no example may become newly rejected.
# Run the same script with --corpus <gfortran.dg> for the full conformance diff.
check-rejection-gate:
	@bash scripts/corpus_rejection_gate.sh \
	    --out build/corpus_rejection_current.tsv \
	    --corpus examples \
	    --baseline test/fixtures/corpus_rejection_baseline.tsv

# Check repository Markdown files for broken relative links
check-doc-links:
	@python3 scripts/check_doc_links.py

# Check project root cleanliness (issue #2148)
check-root-cleanliness:
	@echo "Checking project root for polluting files..."
	@polluting_count=$$(ls -1 | grep -E '\.(mod|lf|f90|txt|a|o)$$' 2>/dev/null | wc -l); \
	if [ "$$polluting_count" -gt 0 ]; then \
	    echo "❌ FAILURE: Project root is polluted with $$polluting_count files"; \
	    echo "Found the following polluting files:"; \
	    ls -1 | grep -E '\.(mod|lf|f90|txt|a|o)$$'; \
	    echo ""; \
	    echo "Project root must be clean - see issue #2148"; \
	    echo "Please clean up: rm -f *.mod *.a *.o *.lf *.f90 *.txt"; \
	    exit 1; \
	else \
	    echo "✅ Project root is clean (0 polluting files)"; \
	fi

clean:
	fpm clean --all
	@echo "Cleaning root directory artifacts..."
	@find . -maxdepth 1 -name "*.mod" -type f -delete
	@find . -maxdepth 1 -name "*.smod" -type f -delete
	@find . -maxdepth 1 -name "*.o" -type f -delete
	@find . -maxdepth 1 -name "*.a" -type f -delete
	@find . -maxdepth 1 -name "*.lf" -type f -delete
	@find . -maxdepth 1 -name "*.f90" -type f -delete
	@find . -maxdepth 1 -name "*.txt" -type f -delete
	@rm -f a.out
	@echo "Clean complete."

# Help target
help:
	@echo "Available targets:"
	@echo "  make          - Build the project (default)"
	@echo "  make build    - Build the project"
	@echo "  make test     - Run tests"
	@echo "  make test-small-stack [TEST_STACK_KB=1024] - Run tests with small stack"
	@echo "  make check-duplication - Check for test duplication violations"
	@echo "  make check-duplication-gate - Negative control for the duplication gate"
	@echo "  make check-rejection-gate - Fail on newly rejected corpus files"
	@echo "  make check-doc-links - Check repository Markdown for broken relative links"
	@echo "  make check-root-cleanliness - Check project root for polluting files (issue #2148)"
	@echo "  make clean    - Clean build artifacts"
