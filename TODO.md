# Expression Parser Modernization (2025 Roadmap)

## Goals
- Replace the recursive descent expression parser with a table-driven, stack-safe Pratt parser.
- Eliminate legacy helpers and duplicated traversal logic across parser, standardizer, and validation layers.
- Align memory usage with arena slabs to guarantee deterministic stack consumption on all platforms (Windows pipe CI foremost).
- Maintain or improve throughput across existing system/regression suites.

## Work Breakdown

1. **Legacy Audit & Removal**
   - [ ] Scan parser, standardizer, and utilities for unused recursive helpers (`parse_*`, `traverse_*`, legacy validation shims).
   - [ ] Drop stale tests/docs covering removed behaviour; replace with canonical SOT parser expectations.

2. **Iterative Pratt Core**
   - [x] Design operator tables (binding powers, prefix/infix handlers).
   - [x] Allocate contiguous SoA token views for parser hot paths.
   - [x] Implement iterative Pratt loop with explicit operand/operator stacks (spill to arena when exceeding on-stack capacity).
   - [x] Integrate unary/postfix handling without recursion (component access, array subscripts, call dispatch).

3. **Pipeline Integration**
   - [ ] Wire new parser through `frontend_parsing` and CLI transform; remove compatibility glue.
   - [ ] Update validation to share token buffers; delete redundant rescan code.
   - [ ] Ensure AST emission stays stable; adjust standardizer/type utilities as needed.

4. **Verification & Performance**
   - [x] Expand deep-expression regression tests (Windows + low-stack Linux) to guard against regressions.
   - [ ] Run full `RUN_SYSTEM_TESTS=1` suites; capture perf baselines.
   - [ ] Document final architecture in `DOCS/` and remove obsolete notes.

## Acceptance Criteria
- No recursive expression parsing remains in the tree.
- CLI + system tests pass on Windows CI without stack reserve hacks.
- Deep nested expression regression (≥4k nesting) parses successfully.
- Perf delta within ±5% on representative workloads.
