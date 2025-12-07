.PHONY: all build test test-small-stack check-duplication check-root-cleanliness clean help libfortfront.a

# Default target
all: build

# Build the project (convenience wrapper)
build:
	fpm build

libfortfront.a:
	@echo "Building fortfront static library..."
	fpm build
	@lib_dir=$$(find build/gfortran_* -type f -name 'libfortfront.a' 2>/dev/null | head -n 1 | xargs dirname); \
	if [ -z "$$lib_dir" ]; then \
	    echo "ERROR: no libfortfront.a produced by fpm build"; \
	    exit 1; \
	fi; \
	echo "Found library build directory: $$lib_dir"; \
	mod_dir=$$(find build/gfortran_* -maxdepth 1 -type f -name '*.mod' 2>/dev/null | head -n 1 | xargs dirname); \
	if [ -z "$$mod_dir" ]; then \
	    echo "ERROR: no module files produced by fpm build"; \
	    exit 1; \
	fi; \
	rm -rf build/fortfront_modules; \
	mkdir -p build/fortfront_modules; \
	find "$$mod_dir" -maxdepth 1 -name '*.mod' -exec cp {} build/fortfront_modules/ \; ; \
	if ! ls build/fortfront_modules/*.mod >/dev/null 2>&1; then \
	    echo "ERROR: no module files copied into build/fortfront_modules"; \
	    exit 1; \
	fi; \
	rm -f libfortfront.a; \
	cp "$$lib_dir/libfortfront.a" libfortfront.a


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
	@echo "  make check-root-cleanliness - Check project root for polluting files (issue #2148)"
	@echo "  make clean    - Clean build artifacts"
