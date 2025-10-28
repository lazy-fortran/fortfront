# Snapshot Tests

This directory contains snapshot tests for the fortfront transpiler. Snapshot testing provides a simple way to verify that code transformations produce the expected output.

## Structure

Each test stores its canonical source in `examples/` and the expected snapshot in `test/snapshots/cases/`:
- Input file: `examples/lf/<name>.lf` or `examples/f90/<name>.f90`
- Expected output: `test/snapshots/cases/<name>.expected`

## Running Tests

```bash
# Run all snapshot tests
./test/run_snapshots.sh

# Run with verbose output
./test/run_snapshots.sh --verbose

# Update expected outputs after code changes
./test/run_snapshots.sh --update
```

## Adding New Tests

1. Add or update the canonical input under `examples/`:
   ```bash
   $EDITOR examples/lf/my_test.lf
   ```

2. Generate the expected output snapshot:
   ```bash
   ./test/run_snapshots.sh --update
   ```

3. Review the generated `.expected` file:
   ```bash
   git diff test/snapshots/cases/
   ```

4. If correct, commit the example and snapshot:
   ```bash
   git add examples/lf/my_test.lf test/snapshots/cases/my_test.expected
   git commit -m "test: add snapshot test for feature X"
   ```

## Updating Tests After Changes

When you modify the transpiler and output format changes:

1. Run tests to see failures:
   ```bash
   ./test/run_snapshots.sh
   ```

2. Review the differences carefully

3. If changes are intentional, update snapshots:
   ```bash
   ./test/run_snapshots.sh --update
   ```

4. Review with git diff to ensure changes are correct:
   ```bash
   git diff test/snapshots/cases/
   ```

5. Commit the updated `.expected` files

## Current Test Coverage

- `examples/lf/assignment_simple.lf` - Basic variable assignments
- `examples/lf/array_constructor.lf` - Array literal syntax
- `examples/lf/array_operations.lf` - Array arithmetic
- `examples/lf/do_loop.lf` - DO loop constructs
- `examples/lf/function_simple.lf` - Function definitions
- `examples/lf/if_statement.lf` - Conditional statements
- `examples/lf/logical_ops.lf` - Logical operators
- `examples/lf/nested_loops.lf` - Nested loop structures
- `examples/lf/string_concat.lf` - String concatenation
- `examples/f90/subroutine_simple.f90` - Subroutine definitions
