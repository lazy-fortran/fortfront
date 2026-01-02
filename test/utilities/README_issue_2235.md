# Manual Testing Guide for Issue #2235

## Issue Summary
Large literal initializations previously caused fortfront to crash with segmentation faults when memory allocation failed. The fix adds proper allocation failure checking with helpful error messages.

## Manual Reproduction (Before Fix)
```bash
# Generate a moderately sized program (80k integers)
python3 - <<'PY'
from pathlib import Path
n = 80000
vals = ', '.join(str(i) for i in range(n))
Path('/tmp/memory_stress.f90').write_text(
    f"program memory_stress\n  implicit none\n  integer :: data({n}) = (/ {vals} /)\n  print *, data(1)\nend program memory_stress\n"
)
PY

# Run under memory pressure (adjust downward to trigger faster)
ulimit -v 120000  # ~120 MB cap
build/gfortran_*/app/fortfront /tmp/memory_stress.f90
```

## Expected Behavior (After Fix)

### Before Fix:
- Process dies with segmentation fault
- Runtime prints: `Could not print backtrace: mmap, errno: 12`
- No helpful error message
- CLI exits with cryptic OS error

### After Fix:
- Process detects allocation failure
- Prints helpful error message:
  - `Failed to allocate input buffer (N bytes)` or
  - `Failed to allocate memory for input expansion (N bytes)` or
  - `Failed to allocate final text buffer (N bytes)`
- Exits cleanly with status code 5
- No segmentation fault

## Status Codes

- `0` - Success
- `1` - Missing filename
- `2` - Cannot open file
- `3` - I/O error while reading
- `4` - Input exceeds maximum size (10 MB limit)
- `5` - **NEW: Memory allocation failure** (this fix)

## Testing the Fix

1. **Build fortfront:**
   ```bash
   fpm build
   ```

2. **Test with normal input (should work):**
   ```bash
   echo "x = 42" | build/gfortran_*/app/fortfront
   ```

3. **Test with memory pressure (should fail gracefully):**
   ```bash
   # Generate large file
   python3 - <<'PY'
   from pathlib import Path
   n = 80000
   vals = ', '.join(str(i) for i in range(n))
   Path('/tmp/memory_stress.f90').write_text(
       f"program memory_stress\n  implicit none\n  integer :: data({n}) = (/ {vals} /)\n  print *, data(1)\nend program memory_stress\n"
   )
   PY

   # Test with memory limit
   ulimit -v 120000
   build/gfortran_*/app/fortfront /tmp/memory_stress.f90
   echo "Exit status: $?"
   ```

4. **Expected output with memory limit:**
   - Clean error message (not segfault)
   - Exit status: 5
   - Message indicates allocation failure with byte count

## Implementation Details

The fix adds `stat=` checking to all `allocate` statements in:
- `app/fortfront.f90`: Main CLI input reading functions
  - `read_all_from_unit()` - Initial buffer allocation
  - `append_chunk()` - Buffer expansion during reading
  - `append_newline()` - Newline buffer expansion
- `test/common/read_example.inc`: Test helper (includes CLI reader helpers)

All allocation failures now:
1. Check `alloc_stat /= 0`
2. Print descriptive error message with byte count
3. Set `status = 5`
4. Return immediately (no dereferencing null/uninitialized memory)
