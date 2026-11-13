# FortFront roadmap

FortFront aims to interpret standard Fortran and Lazy Fortran reliably, expose
source-backed semantic facts to downstream tools, and diagnose unsupported
input clearly. Its [current support boundaries](docs/STATUS.md) apply to all
of these goals.

The next work is to extend Fortran 2023 declarations, expressions, I/O,
intrinsic signatures and coarray facts; resolve valid programs currently
rejected by the frontend; and preserve type, kind, rank, binding and ownership
information for compiler consumers. Conditional expressions and conditional
arguments still need their evaluation semantics implemented before they can
be accepted.

The [issue tracker](https://github.com/lazy-fortran/fortfront/issues) holds
reproducers and acceptance criteria. Each added feature needs an independent
oracle and useful unsupported boundaries. Internal representation changes are
justified by supported behavior or a concrete consumer need.
