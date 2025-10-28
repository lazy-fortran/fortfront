.PHONY: all build test test-small-stack check-duplication clean help

# Default target
all: build

# Build the project (convenience wrapper)
build:
	fpm build



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

clean:
	fpm clean --all

# Help target
help:
	@echo "Available targets:"
	@echo "  make          - Build the project (default)"
	@echo "  make build    - Build the project"
	@echo "  make test     - Run tests"
	@echo "  make test-small-stack [TEST_STACK_KB=1024] - Run tests with small stack"
	@echo "  make check-duplication - Check for test duplication violations"
	@echo "  make clean    - Clean build artifacts"
