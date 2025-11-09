.PHONY: all build test test-small-stack check-duplication check-root-cleanliness clean help

# Default target
all: build

# Build the project (convenience wrapper)
build:
	fpm build

libfortfront.a:
	@echo "Building fortfront static library..."
	fpm build
	@lib_dir=$$(find build/gfortran_* -type d -name fortfront 2>/dev/null | head -n 1 | xargs dirname); \
	if [ -z "$$lib_dir" ]; then \
	    echo "ERROR: no fpm build artifacts found with fortfront subdirectory"; \
	    exit 1; \
	fi; \
	echo "Found library build directory: $$lib_dir"; \
	rm -rf fortfront_modules; \
	mkdir -p fortfront_modules; \
	find "$$lib_dir" -maxdepth 1 -name '*.mod' -exec cp {} fortfront_modules/ \; ; \
	objs=$$(find "$$lib_dir/fortfront" \( -name 'src_*.o' -o -name 'build_dependencies_*.o' \) ); \
	if [ -z "$$objs" ]; then \
	    echo "ERROR: no object files found in $$lib_dir/fortfront"; \
	    exit 1; \
	fi; \
	rm -f libfortfront.a; \
	ar rcs libfortfront.a $$objs


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
