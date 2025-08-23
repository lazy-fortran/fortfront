.PHONY: all build test coverage clean clean-coverage clean-test-artifacts help install libfortfront.a example

# Default target
all: build

# Build the project
build:
	fpm build --flag "-cpp -fmax-stack-var-size=65536"

# Create libfortfront.a in project root for external linking
libfortfront.a: build
	@echo "=== Creating libfortfront.a and collecting modules ==="
	@LATEST_BUILD=$$(find build -name "libfortfront.a" -type f -printf "%T@ %p\n" | sort -n | tail -1 | cut -d' ' -f2-); \
	if [ -n "$$LATEST_BUILD" ]; then \
		cp "$$LATEST_BUILD" ./libfortfront.a; \
		echo "libfortfront.a copied from $$LATEST_BUILD"; \
	else \
		echo "Error: libfortfront.a not found in build directory"; \
		exit 1; \
	fi
	@echo "Collecting Fortran module files..."
	@mkdir -p fortfront_modules
	@for mod_dir in $$(find build -name "*.mod" -type f -exec dirname {} \; | sort -u); do \
		for mod in "$$mod_dir"/*.mod; do \
			if [ -f "$$mod" ]; then \
				MOD_NAME=$$(basename "$$mod"); \
				if [[ ! "$$MOD_NAME" =~ ^(stdlib_|json_|iso_) ]]; then \
					cp "$$mod" fortfront_modules/ 2>/dev/null || true; \
				fi; \
			fi; \
		done; \
	done
	@echo "Module files collected from all build directories in fortfront_modules/"

# Installation variables
PREFIX ?= /usr/local
LIBDIR ?= $(PREFIX)/lib
INCLUDEDIR ?= $(PREFIX)/include/fortfront
PKGCONFIGDIR ?= $(LIBDIR)/pkgconfig

# Install libfortfront.a and module files
install: libfortfront.a
	@echo "=== Installing libfortfront.a and module files ==="
	install -d $(DESTDIR)$(LIBDIR)
	install -d $(DESTDIR)$(INCLUDEDIR)
	install -d $(DESTDIR)$(PKGCONFIGDIR)
	install -m 644 libfortfront.a $(DESTDIR)$(LIBDIR)/
	@echo "Installing Fortran module files..."
	@for mod_dir in $$(find build -name "*.mod" -type f -exec dirname {} \; | sort -u); do \
		find "$$mod_dir" -name "*.mod" -exec install -m 644 {} $(DESTDIR)$(INCLUDEDIR)/ \; 2>/dev/null || true; \
	done
	@echo "Module files installed from all build directories"
	@echo "Generating pkg-config file..."
	@echo "prefix=$(PREFIX)" > $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "exec_prefix=\$${prefix}" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "libdir=\$${exec_prefix}/lib" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "includedir=\$${prefix}/include/fortfront" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "Name: fortfront" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "Description: Core analysis frontend for lazy fortran - lexer, parser, semantic analysis, AST operations" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "Version: 0.1.0" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "Libs: -L\$${libdir} -lfortfront" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "Cflags: -I\$${includedir}" >> $(DESTDIR)$(PKGCONFIGDIR)/fortfront.pc
	@echo "Installation completed successfully"

# Run tests
test:
	fpm test

# Build and run example external tool
example: libfortfront.a
	@echo "=== Building example external tool ==="
	@mkdir -p examples
	@gfortran -fmax-stack-var-size=65536 -I fortfront_modules/ examples/external_tool_example.f90 libfortfront.a -o examples/external_tool_example
	@echo "=== Running example ===" 
	@./examples/external_tool_example

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
	rm -f libfortfront.a
	rm -rf fortfront_modules/

# Clean coverage files only
clean-coverage:
	rm -rf coverage_html/
	rm -f coverage.info coverage_filtered.info coverage.xml
	rm -f *.gcov *.gcda *.gcno
	find . -name "*.gcov" -o -name "*.gcda" -o -name "*.gcno" -delete 2>/dev/null || true

# Clean test artifacts (temporary files created by tests)
clean-test-artifacts:
	@echo "Cleaning test artifacts..."
	rm -f *.lf *.f90 *.json 2>/dev/null || true
	rm -f coverage.*.html coverage.info coverage_filtered.info coverage.css 2>/dev/null || true
	rm -f intent_*.f90 valid_*.f90 2>/dev/null || true
	@echo "Test artifacts cleaned"

# Help target
help:
	@echo "Available targets:"
	@echo "  make          - Build the project (default)"
	@echo "  make build    - Build the project"
	@echo "  make libfortfront.a - Create static library and collect modules for external linking"
	@echo "  make install  - Install libfortfront.a, module files, and pkg-config"
	@echo "  make test     - Run tests"
	@echo "  make example  - Build and run example external tool"
	@echo "  make coverage - Generate coverage report with lcov"
	@echo "  make clean    - Clean all build and test artifacts"
	@echo "  make clean-coverage - Clean coverage files only"
	@echo "  make clean-test-artifacts - Clean test temporary files only"
	@echo "  make help     - Show this help message"
