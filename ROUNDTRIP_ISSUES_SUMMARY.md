# GitHub Issues Created for GFortran Roundtrip Failures

**Date**: 2025-11-21  
**Source**: GFortran DejaGNU roundtrip check analysis

## Epic Tracking Issue

**#2419** - Epic: Improve gfortran roundtrip success rate to 75%
- Tracks overall progress from 67.6% → 75%
- Links all related issues
- Timeline: 3 months

## Critical Issues (P1)

### Parser Crashes

**#2413** - Parser crash: interface blocks with function results in type specifications
- **Impact**: 9 tests
- **Problem**: Parser fails when function results are used in type specs
- **Example**: `character(compute_size(array)) :: result`
- **Labels**: bug, parser, interfaces, p1, roundtrip

**#2416** - Parser crash: I/O statement format parsing issues  
- **Impact**: 4 tests
- **Problem**: Crashes on complex READ/WRITE format specifications
- **Example**: Variable format strings, NAMELIST
- **Labels**: bug, parser, p1, roundtrip

### Codegen Bugs

**#2414** - Codegen bug: round-trip output fails to compile
- **Impact**: 94 tests (5.6% of failures)
- **Problem**: Fortfront emits invalid Fortran code
- **Example**: Various patterns with pointers, allocatables, procedures
- **Labels**: bug, codegen, p1, roundtrip

**#2415** - Codegen bug: emitted DATA statements fail to parse
- **Impact**: 8 tests
- **Problem**: Second pass can't parse DATA statements from first pass
- **Example**: Round-trip fails on own output
- **Labels**: bug, codegen, parser, p1, roundtrip

## High/Medium Priority Issues (P2)

**#2417** - Performance: infinite loop in array intrinsic processing
- **Impact**: 26 tests
- **Problem**: Hangs on MINLOC, MAXLOC, RESHAPE, EOSHIFT
- **Example**: Complex array operations with masks
- **Labels**: bug, performance, p2, roundtrip

**#2418** - Codegen: emitted code is harder to parse than input
- **Impact**: 14 tests
- **Problem**: Second pass times out parsing own output
- **Example**: Codegen increases complexity unnecessarily
- **Labels**: bug, codegen, performance, p2, roundtrip

## Key Requirements (ALL ISSUES)

### MANDATORY: No Code Copying

All issues require:
1. ✅ Create ORIGINAL test cases
2. ❌ DO NOT copy GCC DejaGNU test code
3. ✅ Invent similar examples demonstrating the pattern
4. ✅ Keep examples minimal (< 50 lines)
5. ✅ Add to fortfront test suite
6. ✅ CI validation

### Why This Matters

1. **Legal**: GCC test suite has specific licensing
2. **Quality**: Forces understanding of the actual issue
3. **Maintainability**: Our tests match our codebase patterns
4. **Minimal**: GCC tests often have extra complexity

## Expected Impact

### Critical Issues (13 parser + 10 codegen = 23 tests)
- Current success: 67.6%
- After critical fixes: **68.1%** (+0.5%)

### With High Priority (94 + 26 = 120 tests)  
- After high priority: **70.9%** (+3.3%)

### With Medium Priority (14 tests)
- After medium priority: **71.2%** (+3.6%)

### To Reach 75% Target
- Need 404 more passing tests
- Critical + High + Medium = 157 tests fixed
- Need additional 247 test improvements
- Focus on COMPILE_FAIL_REF patterns

## Implementation Strategy

### Month 1: Critical Issues
1. Fix parser crashes (#2413, #2416)
2. Fix DATA statement bug (#2415)
3. Investigate codegen invalid output (#2414)
4. Target: 68% success rate

### Month 2: High Priority
1. Complete codegen fixes (#2414)
2. Profile and fix infinite loops (#2417)
3. Target: 71% success rate

### Month 3: Polish
1. Simplify codegen output (#2418)
2. Review COMPILE_FAIL_REF patterns
3. Additional improvements
4. Target: 75% success rate

## Tracking Progress

### Run Full Check
```bash
python3 scripts/run_gfortran_roundtrip.py --jobs 32
```

### Metrics to Track
- Success rate (pass / testable)
- Fatal crashes (target: < 20)
- Timeouts (target: < 15)
- Invalid codegen (target: 0)

### Monthly Reporting
- Update epic issue #2419
- Document success rate change
- Identify next priorities

## Resources

- **Analysis**: `logs/gfortran_roundtrip_analysis.md`
- **Results**: `logs/gfortran_dejagnu_roundtrip_results.jsonl`
- **Summary**: `logs/gfortran_dejagnu_roundtrip_results_summary.json`
- **Script**: `scripts/run_gfortran_roundtrip.py`

## Test Case Examples (Original, Not Copied)

Each issue includes example patterns like:

### Interface Function Result (#2413)
```fortran
interface
  pure integer function compute_size(arr)
    integer, intent(in) :: arr(:)
  end function
end interface
character(compute_size(my_array)) :: result_string
```

### DATA Statement (#2415)
```fortran
program test_data
  integer :: arr(3)
  data arr / 1, 2, 3 /
  print *, arr
end program
```

### I/O Format (#2416)
```fortran
program test_io
  integer :: val
  character(10) :: fmt
  fmt = '(I5)'
  read(*, fmt) val
end program
```

These are patterns to demonstrate, NOT code to copy. Actual tests must be original.
