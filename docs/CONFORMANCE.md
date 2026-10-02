# Frontend Conformance

FortFront tracks external frontend coverage without vendoring foreign test
sources. The gate runs source files through `compile_frontend_from_file`,
records parse, semantic-analysis, and round-trip states, then groups failures by
construct and diagnostic pattern.

## Invalid-program validation

The compiler-facing API refuses duplicate type declarations in one lexical
scope, local entities that collide with use-associated derived types, and DO
variables or COMMON members that resolve to a derived type. BLOCK declarations,
procedure dummies, and renamed imports retain their separate binding identities.
Attribute-only DIMENSION, EXTERNAL, and typeless PROCEDURE statements can
supplement a type declaration. DATA expansion nodes retain the DATA validator's
initialization diagnostics.
Named constants cannot be assignment targets or DO variables; keyword actuals
remain expression contexts. Intrinsic LEN and LEN_TRIM references require one or
two arguments. END PROGRAM names and END DO construct names must match their
opening statements, including names in nested procedure and loop bodies.

Named DO constructs can occur inside several unnamed loops without changing
nesting depth. Their complete closing names remain part of each statement slice.
A dotted operator immediately after a numeric literal terminates that literal;
decimal points, scientific exponents, and kind suffixes retain their normal
numeric spelling.
Operator names are case insensitive in the parsed expression tree.

Prefix minus preserves its operand's numeric type, kind, and rank. The parser
retains its unary origin when representing it through a synthetic-zero binary
node; that zero does not promote the operand. Ordinary binary subtraction keeps
its usual numeric promotion.

Focused API tests cover these rules; the downstream compiler's
`tools/test_name_namespace_collision_parity.py` compares acceptance and executable
stdout with gfortran and permits no silently accepted invalid programs.

Identifier hashes use bounded modular arithmetic with the same stored hash
values. A scope stack owns and finalizes its identifier storage; copied stacks
own independent storage, and their scope environments borrow from that copy.
Sanitized tests cover repeated compilation and scope-stack copying and reset.

## Suites

- `gfortran-dg`: GCC DejaGNU Fortran tests. Set `FF_GFORTRAN_DG_DIR` to the
  `gfortran.dg` directory. The default is `../gcc/gcc/testsuite/gfortran.dg`
  relative to this repository.
- `lfortran`: lfortran integration tests. Set `FF_LFORTRAN_DIR` to the lfortran
  source root. The default is `../lfortran` relative to this repository.

Absent suites print `SKIP` and exit 0. Local CI and normal development do not
need a GCC or lfortran checkout.

## Run

```sh
scripts/run_frontend_conformance.sh --suite all --report /tmp/ff_frontend.jsonl
```

To run one suite:

```sh
FF_GFORTRAN_DG_DIR=/path/to/gcc/gcc/testsuite/gfortran.dg \
  scripts/run_frontend_conformance.sh --suite gfortran-dg \
  --report /tmp/ff_gfortran_dg.jsonl

FF_LFORTRAN_DIR=/path/to/lfortran \
  scripts/run_frontend_conformance.sh --suite lfortran \
  --report /tmp/ff_lfortran.jsonl
```

The wrapper accepts `--gcc-root` and `--lfortran-root` as explicit overrides.
It forwards the other options to `scripts/run_gfortran_roundtrip.py`, for
example `--max-tests 50`, `--jobs 1`, `--timeout 0.2`, `--fortfront`, or
`--frontend-probe`.

## Reports

Each per-file JSONL record includes:

- `suite`
- `file`, relative to the suite root
- `parse_ok` and `parse_state`
- `semantic_ok` and `sema_state`
- `roundtrip_state`
- `source_keywords` and `source_patterns`

The runner also writes `<report>_summary.json`. That summary contains totals,
the failure digest, path and keyword heatmaps, and per-category construct
counts. The top buckets feed the Fortran 2023 frontend tracker.

## Xfail Baseline

Manifests live under `test/conformance/`:

- `frontend_xfail_gfortran_dg.txt`
- `frontend_xfail_lfortran.txt`

Each non-comment line is one suite-relative path. A listed file that still fails
counts as XFAIL. A listed file that passes is reported as XPASS and should be
removed from the manifest in the same change that adds support.

Do not commit GCC or lfortran sources. The repository owns only manifests,
scripts, docs, and local smoke tests.
