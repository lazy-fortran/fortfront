# Utilities Tests

## Purpose

This directory contains tests for utility modules used across fortfront, such as
debug tracing/profiling, environment handling, and string helpers.

## File Index

| File | Description |
| --- | --- |
| `string_utils.f90` | Test helpers for string utilities. |
| `test_cli_io_allocation_failure.f90` | CLI I/O allocation failure handling tests. |
| `test_cli_io_large_input.f90` | CLI large input handling tests. |
| `test_debug_trace_profile.f90` | Profiling stats collection test for `debug_trace`. |
| `test_debug_trace_profile_mismatch.f90` | Profiling enter/leave name mismatch validation test. |
| `test_debug_trace_profile_report_sorted.f90` | Profile report output is sorted by total time. |
| `test_environment_value.f90` | Environment value parsing tests. |
| `test_shell_commands_windows_quotes.f90` | Windows quoting tests for shell command helpers. |
| `test_type_string_utils.f90` | Type string utility tests. |

## Key Concepts

- Profiling tests use `trace_set_profile_enabled()` to enable internal profiling
  without relying on environment variables.
- Tests are small executables discovered by fpm and use `stop 1` on failure.

## Dependencies

- `debug_trace` and other utility modules under `src/utilities/`.
