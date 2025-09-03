.PHONY: all build test lint clean clean-test-artifacts clean-logs help install libfortfront.a example setup-githooks prune-build-cache

# Number of newest fpm build cache directories to keep under build/
# Can be overridden, e.g.: `make PRUNE_KEEP=2 prune-build-cache`
PRUNE_KEEP ?= 2
TEST_TIMEOUT ?= 120s

# Default target
all: build

# Build the project
build:
	# Ensure stray root artifacts (e.g., *.f90 from samples/tests) don't pollute builds
	$(MAKE) -s clean-test-artifacts
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
	# Ensure a clean workspace before running tests and remove artifacts after
	$(MAKE) -s clean-logs
	$(MAKE) -s clean-test-artifacts
	# Enforce production error handling policy (no 'error stop' in src/)
	scripts/check_no_error_stop.sh
	@echo "Running tests with timeout $(TEST_TIMEOUT)"
	@sh -c 'scripts/with_timeout.sh $(TEST_TIMEOUT) fpm test; rc=$$?; $(MAKE) -s clean-test-artifacts; exit $$rc'

# Build and run example external tool
example: libfortfront.a
	@echo "=== Building example external tool ==="
	@mkdir -p examples
	@gfortran -fmax-stack-var-size=65536 -I fortfront_modules/ examples/external_tool_example.f90 libfortfront.a -o examples/external_tool_example
	@echo "=== Running example ===" 
	@./examples/external_tool_example

# Clean build artifacts and test artifacts
clean: clean-test-artifacts
	fpm clean --all
	rm -f libfortfront.a
	rm -rf fortfront_modules/
	# Also remove any stale fpm hash build directories to prevent bloat
	find build -mindepth 1 -maxdepth 1 -type d -name 'gfortran_*' -exec rm -rf {} + 2>/dev/null || true
	$(MAKE) -s clean-logs

# Clean test artifacts (temporary files created by tests)
clean-test-artifacts:
	@echo "Cleaning test artifacts..."
	rm -f *.lf *.f90 *.json 2>/dev/null || true
	rm -f intent_*.f90 valid_*.f90 2>/dev/null || true
	# Additional test-generated files (mirrors .gitignore entries)
	rm -f test_*.md test_*.txt *_test.md *_test.txt *_test.f *_test.f90 \
		test_results* test_output* 2>/dev/null || true
	rm -f *.tmp *.temp 2>/dev/null || true
	rm -f test_go_style_simple test_minimal_components \
		test_simple_plugin_registry test_standalone_events test_output \
		debug_error_test debug_*_test 2>/dev/null || true
	@echo "Test artifacts cleaned"

# Clean logs to prevent accumulation
clean-logs:
	@echo "Cleaning old log files..."
	rm -f *.log *.err *.out 2>/dev/null || true
	find logs -type f -name '*.log' -delete 2>/dev/null || true
	@echo "Logs cleaned"

# Help target
help:
	@echo "Available targets:"
	@echo "  make          - Build the project (default)"
	@echo "  make build    - Build the project"
	@echo "  make libfortfront.a - Create static library and collect modules for external linking"
	@echo "  make install  - Install libfortfront.a, module files, and pkg-config"
	@echo "  make test     - Run tests"
	@echo "  make lint     - Enforce source file-length (line count) limits"
	@echo "  make example  - Build and run example external tool"
	@echo "  make clean    - Clean all build and test artifacts"
	@echo "  make clean-test-artifacts - Clean test temporary files only"
	@echo "  make prune-build-cache - Prune old fpm hash dirs; keep newest via PRUNE_KEEP (default 2)"
	@echo "  make help     - Show this help message"
	@echo "  make setup-githooks - Configure repo to use .githooks/"

# Configure git to use the repo's githooks directory
setup-githooks:
	@git config core.hooksPath .githooks
	@echo "Configured git core.hooksPath to .githooks"

# Prune old fpm build hash directories (build/gfortran_*), keeping only the newest PRUNE_KEEP
prune-build-cache:
	@keep=$${PRUNE_KEEP:-2}; if [ -d build ]; then \
	  echo "Pruning fpm build cache (keeping $$keep newest)"; \
	  find build -mindepth 1 -maxdepth 1 -type d -name 'gfortran_*' -printf '%T@ %p\n' \
	    | sort -nr \
	    | awk -v k="$$keep" 'NR>k{print $$2}' \
	    | xargs -r rm -rf; \
	fi; true

# Lint: enforce soft/hard file length (line count) limits for sources
lint:
	@scripts/check_line_lengths.sh
