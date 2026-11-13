# Changelog

## Unreleased

- Diagnose unsupported Fortran 2023 conditional expressions and conditional
  arguments instead of silently accepting a changed expression.
- Identify character substrings with runtime bounds correctly in compiler
  queries, including substrings of character literals.
- Make `debug_ast FILE` inspect the supplied source and report read or parse
  failures on standard error.
- Initialize nested type components explicitly in the type-string utility
  regression and preserve borrowed node targets in the derived-type parser
  regression so those tests compile with LLVM Flang.
- Keep the external conformance wrapper usable with no forwarded arguments on
  systems using Bash 3.2.
- Require concrete acquisition evidence before exposing exact dynamic type or
  binding identity from an owned-array `CLASS IS` guard.
- Preserve filenames with spaces in the corpus rejection gate and fail if
  probe execution leaves an incomplete report.
- Expose the character-type dependency to fpm so clean builds schedule it
  before program emission.
- Include all public modules in the standalone static library build.
