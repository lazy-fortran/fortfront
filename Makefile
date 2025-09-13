.PHONY: all build test clean help

# Default target
all: build

# Build the project (convenience wrapper)
build:
	fpm build



# Run tests (convenience wrapper)
test:
	fpm test

clean:
	fpm clean --all

# Help target
help:
	@echo "Available targets:"
	@echo "  make          - Build the project (default)"
	@echo "  make build    - Build the project"
	@echo "  make test     - Run tests"
	@echo "  make clean    - Clean build artifacts"
