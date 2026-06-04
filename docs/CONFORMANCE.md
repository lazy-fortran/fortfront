# Frontend Conformance

FortFront tracks external frontend coverage without vendoring foreign test
sources. The gate runs source files through `compile_frontend_from_file`,
records parse, semantic-analysis, and round-trip states, then groups failures by
construct and diagnostic pattern.

## Suites

- `gfortran-dg`: GCC DejaGNU Fortran tests. Set `FF_GFORTRAN_DG_DIR` to either a
  GCC source root or a direct `gfortran.dg` directory. The default is `../gcc`
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

The wrapper forwards other options to `scripts/run_gfortran_roundtrip.py`, for
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
