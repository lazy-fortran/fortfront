# Snapshot Tests

This directory contains snapshot tests for the fortfront transpiler. Snapshot testing provides a simple way to verify that code transformations produce the expected output.

## Structure

Each test consists of two files:
- Input file: `.lf` or `.f90` file containing Fortran code
- Expected output: `.expected` file containing the transpiled result

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

1. Create input file in `test/snapshots/`:
   ```bash
   echo 'x = 5' > test/snapshots/my_test.lf
   ```

2. Generate expected output:
   ```bash
   ./test/run_snapshots.sh --update
   ```

3. Review the generated `.expected` file:
   ```bash
   git diff test/snapshots/
   ```

4. If correct, commit both files:
   ```bash
   git add test/snapshots/my_test.lf test/snapshots/my_test.expected
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
   git diff test/snapshots/
   ```

5. Commit the updated `.expected` files

## Current Test Coverage

- `assignment_simple.lf` - Basic variable assignments
- `array_constructor.lf` - Array literal syntax
- `array_operations.lf` - Array arithmetic
- `do_loop.lf` - DO loop constructs
- `function_simple.lf` - Function definitions
- `if_statement.lf` - Conditional statements
- `logical_ops.lf` - Logical operators
- `nested_loops.lf` - Nested loop structures
- `string_concat.lf` - String concatenation
- `subroutine_simple.f90` - Subroutine definitions
