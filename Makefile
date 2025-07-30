.PHONY: all build test coverage clean clean-coverage clean-test-artifacts help

# Default target
all: build

# Build the project
build:
	fpm build

# Run tests
test:
	fpm test

# Generate coverage report
coverage: clean-coverage
	@echo "=== Generating code coverage with lcov ==="
	fpm clean --all
	fpm test --profile debug --flag '-cpp -fprofile-arcs -ftest-coverage -g'
	lcov --capture --directory build/ --output-file coverage.info \
		--rc branch_coverage=1 \
		--ignore-errors inconsistent,mismatch
	lcov --remove coverage.info \
		'build/dependencies/*' \
		'test/*' \
		--output-file coverage_filtered.info \
		--ignore-errors unused
	genhtml coverage_filtered.info --output-directory coverage_html \
		--branch-coverage \
		--legend
	@echo "=== Coverage Summary ==="
	@lcov --summary coverage_filtered.info
	@if command -v lcov_cobertura &> /dev/null; then \
		echo "Generating XML report for CI/CD..."; \
		lcov_cobertura coverage_filtered.info -o coverage.xml; \
	else \
		echo "Note: Install lcov_cobertura (pip install lcov-cobertura) to generate XML reports"; \
	fi
	@echo "Coverage report generated in coverage_html/index.html"

# Clean build artifacts and test artifacts
clean: clean-test-artifacts
	fpm clean --all

# Clean coverage files only
clean-coverage:
	rm -rf coverage_html/
	rm -f coverage.info coverage_filtered.info coverage.xml
	rm -f *.gcov *.gcda *.gcno
	find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno" -delete 2>/dev/null || true

# Clean test artifacts (temporary files created by tests)
clean-test-artifacts:
	@echo "Cleaning test artifacts..."
	rm -f test_*.lf test_*.f90 *.json 2>/dev/null || true
	rm -f coverage.*.html coverage.info coverage_filtered.info coverage.css 2>/dev/null || true
	rm -f intent_*.f90 valid_*.f90 2>/dev/null || true
	@echo "Test artifacts cleaned"

# Help target
help:
	@echo "Available targets:"
	@echo "  make          - Build the project (default)"
	@echo "  make build    - Build the project"
	@echo "  make test     - Run tests"
	@echo "  make coverage - Generate coverage report with lcov"
	@echo "  make clean    - Clean all build and test artifacts"
	@echo "  make clean-coverage - Clean coverage files only"
	@echo "  make clean-test-artifacts - Clean test temporary files only"
	@echo "  make help     - Show this help message"