# Semantic Pipeline Quick Reference

## Essential Imports

```fortran
use semantic_pipeline, only: semantic_pipeline_t, create_pipeline
use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t, &
                             usage_tracker_analyzer_t, source_reconstruction_analyzer_t, &
                             call_graph_analyzer_t, control_flow_analyzer_t, interface_analyzer_t
```

## Basic Workflow

```fortran
! 1. Create pipeline
type(semantic_pipeline_t) :: pipeline
pipeline = create_pipeline()

! 2. Register analyzers
call pipeline%register_analyzer(analyzer_instance)

! 3. Run analysis
call pipeline%run_analysis(arena, root_node_index)

! 4. Access results
results = pipeline%analyzers(index)%analyzer%get_results()
```

## Core Analyzers

| Analyzer | Purpose | Result Type |
|----------|---------|-------------|
| `symbol_analyzer_t` | Symbol collection | `semantic_context_t` |
| `type_analyzer_t` | Type inference | `semantic_context_t` |
| `scope_analyzer_t` | Scope management | `semantic_context_t` |

## Analysis Plugins

| Analyzer | Fluff Rules | Methods |
|----------|-------------|---------|
| `usage_tracker_analyzer_t` | F006, F007 | `find_unused_variables()`, `find_undefined_variables()` |
| `source_reconstruction_analyzer_t` | F014, F015 | `get_node_source_text()`, `get_line_text()` |
| `call_graph_analyzer_t` | P001, P007 | Analysis-specific methods |
| `control_flow_analyzer_t` | P002-P004 | Analysis-specific methods |
| `interface_analyzer_t` | F009 | Analysis-specific methods |

## Pipeline Methods

| Method | Description |
|--------|-------------|
| `%register_analyzer(analyzer)` | Add analyzer to pipeline |
| `%run_analysis(arena, node)` | Execute all analyzers |
| `%get_analyzer_count()` | Get number of registered analyzers |
| `%clear_analyzers()` | Remove all analyzers |

## Analyzer Methods

| Method | Description |
|--------|-------------|
| `%analyze(context, arena, node)` | Run analysis |
| `%get_results()` | Get analysis results |
| `%get_name()` | Get analyzer name |

## Fluff Integration Template

```fortran
! Complete fluff pipeline setup
fluff_pipeline = create_pipeline()
call fluff_pipeline%register_analyzer(usage_tracker_analyzer_t())      ! F006, F007
call fluff_pipeline%register_analyzer(source_reconstruction_analyzer_t()) ! F014, F015
call fluff_pipeline%register_analyzer(call_graph_analyzer_t())         ! P001, P007
call fluff_pipeline%register_analyzer(control_flow_analyzer_t())       ! P002-P004
call fluff_pipeline%register_analyzer(interface_analyzer_t())          ! F009

! Run on parsed Fortran code
call fluff_pipeline%run_analysis(arena, program_node)

! Extract rule-specific results
unused_vars = fluff_pipeline%analyzers(1)%analyzer%find_unused_variables()
```

## Error-Safe Patterns

```fortran
! Always check allocation before access
if (allocated(pipeline%analyzers(i)%analyzer)) then
    results = pipeline%analyzers(i)%analyzer%get_results()
    ! Process results...
end if

! Type-safe result access
select type(results)
type is (usage_analysis_result_t)
    ! Handle usage analysis results
type is (semantic_context_t)
    ! Handle semantic context results
class default
    ! Unknown result type
end select
```

## Performance Tips

- **Reuse pipelines** for multiple files
- **Register only needed analyzers** for your use case
- **Clear intermediate results** if memory is constrained
- **Profile individual analyzers** to identify bottlenecks

## Common Gotchas

- Import all analyzer types you plan to use
- Check `allocated` status before accessing analyzers
- Use `select type` for type-safe result access
- Node indices must be valid (> 0, <= arena%size)
- Empty pipelines are safe but produce no results