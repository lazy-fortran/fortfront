# DESIGN.md: Semantic Analysis Extensibility Framework

## Problem Analysis

The current semantic analysis system has architectural limitations that prevent extensibility:

1. **Monolithic Design**: `semantic_analyzer.f90` handles all analysis in a single 2200-line module
2. **Disconnected Query API**: `semantic_query_api.f90` exists but isn't integrated with the main analysis pipeline
3. **No Extensibility**: Custom analyzers require modifying core code
4. **Memory Issues**: Query API has deep copy problems (issue #196)
5. **Future Blocking**: Issues #191-194 require custom analysis capabilities

## Architectural Vision

### Core Design Principles

1. **Plugin-Based Architecture**: Analyzers as independent, composable modules
2. **Event-Driven Pipeline**: Analyzers subscribe to analysis events and AST traversal hooks
3. **Clean Separation**: Core analysis engine separate from individual analyzer implementations
4. **Query Integration**: Unified interface for accessing all analysis results
5. **Performance First**: Zero-copy data access, minimal overhead for unused analyzers

### High-Level Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Frontend Interface                        │
│              analyze_semantics(arena, prog_index)          │
└─────────────────────────────────────────────────────────────┘
                                │
┌─────────────────────────────────────────────────────────────┐
│                Semantic Analysis Pipeline                   │
│  ┌─────────────────┐  ┌─────────────────┐  ┌──────────────┐ │
│  │   Orchestrator  │──│ Event Dispatcher │──│ Query Engine │ │
│  └─────────────────┘  └─────────────────┘  └──────────────┘ │
└─────────────────────────────────────────────────────────────┘
                                │
        ┌──────────────────────────────────────────────────────┐
        │                Plugin Analyzers                      │
        │ ┌─────────────┐ ┌─────────────┐ ┌─────────────┐     │
        │ │ Core Typing │ │ Flow Control│ │   Custom    │ ... │
        │ │   Analyzer  │ │   Analyzer  │ │  Analyzers  │     │
        │ └─────────────┘ └─────────────┘ └─────────────┘     │
        └──────────────────────────────────────────────────────┘
                                │
┌─────────────────────────────────────────────────────────────┐
│                   Analysis Results Store                    │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐           │
│  │   Types     │ │ Symbol Table│ │ Custom Data │           │
│  └─────────────┘ └─────────────┘ └─────────────┘           │
└─────────────────────────────────────────────────────────────┘
```

## 1. Core Architecture Components

### 1.1 Analysis Pipeline Orchestrator

**Purpose**: Coordinates the overall analysis process and manages analyzer lifecycle.

```fortran
module semantic_orchestrator
    type :: analysis_orchestrator_t
        type(analyzer_registry_t) :: registry
        type(analysis_context_t) :: context
        type(query_engine_t) :: query_engine
    contains
        procedure :: register_analyzer
        procedure :: run_analysis
        procedure :: get_query_interface
    end type
end module
```

**Key Responsibilities**:
- Analyzer registration and discovery
- Analysis phase coordination
- Result aggregation
- Memory management across analyzers

### 1.2 Event-Driven Analysis Engine

**Purpose**: Provides hooks for analyzers to participate in AST traversal and analysis events.

```fortran
module semantic_events
    type :: analysis_event_t
        integer :: event_type
        integer :: node_index
        type(ast_arena_t), pointer :: arena
        type(analysis_context_t), pointer :: context
    end type

    type :: event_dispatcher_t
        type(event_subscriber_t), allocatable :: subscribers(:)
    contains
        procedure :: subscribe
        procedure :: dispatch_event
        procedure :: traverse_ast_with_events
    end type
end module
```

**Event Types**:
- `EVENT_NODE_ENTER`: Before analyzing a node
- `EVENT_NODE_EXIT`: After analyzing a node  
- `EVENT_SCOPE_ENTER`: Entering new scope
- `EVENT_SCOPE_EXIT`: Leaving scope
- `EVENT_ANALYSIS_COMPLETE`: All analysis finished

### 1.3 Unified Query Engine

**Purpose**: Provides efficient, type-safe access to all analysis results without deep copies.

```fortran
module semantic_query_engine
    type :: query_engine_t
        type(result_store_t) :: results
        type(semantic_context_t), pointer :: context
        type(ast_arena_t), pointer :: arena
    contains
        procedure :: query_type_info
        procedure :: query_symbol_info
        procedure :: query_custom_data
        procedure :: is_symbol_defined
        procedure :: get_unused_variables
    end type
end module
```

**Key Features**:
- Direct pointer access (no copying)
- Type-safe result queries
- Backward compatibility with existing API
- Efficient batch operations

## 2. Plugin Analyzer Framework

### 2.1 Base Analyzer Interface

**Purpose**: Defines the contract that all analyzers must implement.

```fortran
module analyzer_interface
    type, abstract :: base_analyzer_t
        character(len=:), allocatable :: analyzer_name
        integer, allocatable :: required_phases(:)
        integer, allocatable :: subscribed_events(:)
    contains
        procedure(analyzer_init_interface), deferred :: initialize
        procedure(analyzer_analyze_interface), deferred :: analyze_node
        procedure(analyzer_finalize_interface), deferred :: finalize_analysis
        procedure(analyzer_query_interface), deferred :: handle_query
    end type

    abstract interface
        subroutine analyzer_init_interface(this, context)
            import :: base_analyzer_t, analysis_context_t
            class(base_analyzer_t), intent(inout) :: this
            type(analysis_context_t), intent(inout) :: context
        end subroutine

        subroutine analyzer_analyze_interface(this, event)
            import :: base_analyzer_t, analysis_event_t
            class(base_analyzer_t), intent(inout) :: this
            type(analysis_event_t), intent(in) :: event
        end subroutine

        subroutine analyzer_finalize_interface(this, context)
            import :: base_analyzer_t, analysis_context_t
            class(base_analyzer_t), intent(inout) :: this
            type(analysis_context_t), intent(inout) :: context
        end subroutine

        function analyzer_query_interface(this, query_type, query_data) result(success)
            import :: base_analyzer_t
            class(base_analyzer_t), intent(in) :: this
            integer, intent(in) :: query_type
            class(*), intent(inout) :: query_data
            logical :: success
        end function
    end interface
end module
```

### 2.2 Core Type Analyzer (Built-in)

**Purpose**: Handles Hindley-Milner type inference (migrated from current semantic_analyzer).

```fortran
module core_type_analyzer
    type, extends(base_analyzer_t) :: core_type_analyzer_t
        type(semantic_context_t) :: type_context
    contains
        procedure :: initialize => core_type_init
        procedure :: analyze_node => core_type_analyze
        procedure :: finalize_analysis => core_type_finalize
        procedure :: handle_query => core_type_query
    end type
end module
```

### 2.3 Control Flow Analyzer (Plugin)

**Purpose**: Analyzes control flow patterns for optimization and validation.

```fortran
module control_flow_analyzer
    type, extends(base_analyzer_t) :: control_flow_analyzer_t
        type(flow_graph_t) :: flow_graph
        logical, allocatable :: reachable_nodes(:)
    contains
        procedure :: initialize => flow_init
        procedure :: analyze_node => flow_analyze
        procedure :: finalize_analysis => flow_finalize
        procedure :: handle_query => flow_query
    end type
end module
```

## 3. Integration Strategy

### 3.1 Backward Compatibility

**Current Interface Preservation**:
```fortran
! frontend.f90 - NO CHANGES to external interface
subroutine analyze_semantics(arena, prog_index)
    ! Internally uses new orchestrator
    type(analysis_orchestrator_t) :: orchestrator
    call orchestrator%register_analyzer(create_core_type_analyzer())
    call orchestrator%run_analysis(arena, prog_index)
end subroutine
```

### 3.2 Query API Integration

**Unified Access Point**:
```fortran
! New unified interface in semantic_query_api.f90
function get_semantic_query_engine(arena, context) result(engine)
    type(ast_arena_t), target, intent(in) :: arena
    type(analysis_context_t), target, intent(in) :: context
    type(query_engine_t) :: engine
    
    ! Direct pointer assignment - NO COPYING
    engine%arena => arena
    engine%context => context
end function
```

### 3.3 Memory Management Strategy

**Zero-Copy Principle**:
- All queries use direct pointer access
- Results stored in arena-allocated structures
- Analyzers register result locations with query engine
- No intermediate copying of large data structures

## 4. Implementation Phases

### Phase 1: Core Infrastructure (Week 1-2)
1. Create `semantic_orchestrator.f90`
2. Implement `analyzer_interface.f90`
3. Create `semantic_events.f90`
4. Design result storage system

### Phase 2: Core Type Analyzer Migration (Week 3-4)
1. Extract type inference logic from `semantic_analyzer.f90`
2. Implement `core_type_analyzer.f90`
3. Integrate with event system
4. Preserve all existing functionality

### Phase 3: Query Engine Integration (Week 5)
1. Implement `semantic_query_engine.f90`
2. Migrate existing query API to use engine
3. Add direct query functions (issue #196 solution)
4. Comprehensive testing

### Phase 4: Plugin Development Framework (Week 6)
1. Create analyzer template generator
2. Implement control flow analyzer as proof-of-concept
3. Document plugin development guidelines
4. Performance benchmarking

### Phase 5: Advanced Features (Week 7-8)
1. Custom analyzer hot-loading
2. Analysis result caching
3. Incremental analysis support
4. Integration with issues #193, #194

## 5. Plugin Development Example

### Creating a Dead Code Analyzer

```fortran
module dead_code_analyzer
    type, extends(base_analyzer_t) :: dead_code_analyzer_t
        logical, allocatable :: live_statements(:)
    contains
        procedure :: initialize => dead_code_init
        procedure :: analyze_node => dead_code_analyze  
        procedure :: finalize_analysis => dead_code_finalize
        procedure :: handle_query => dead_code_query
    end type

contains
    subroutine dead_code_init(this, context)
        class(dead_code_analyzer_t), intent(inout) :: this
        type(analysis_context_t), intent(inout) :: context
        
        this%analyzer_name = "dead_code_analyzer"
        this%subscribed_events = [EVENT_NODE_ENTER, EVENT_ANALYSIS_COMPLETE]
        allocate(this%live_statements(context%arena%size))
        this%live_statements = .false.
    end subroutine

    subroutine dead_code_analyze(this, event)
        class(dead_code_analyzer_t), intent(inout) :: this
        type(analysis_event_t), intent(in) :: event
        
        select case (event%event_type)
        case (EVENT_NODE_ENTER)
            ! Mark reachable statements
            call mark_reachable(this, event%node_index)
        case (EVENT_ANALYSIS_COMPLETE)
            ! Final dead code detection
            call find_dead_code(this, event%arena)
        end select
    end subroutine
end module
```

## 6. Performance Considerations

### 6.1 Lazy Initialization
- Analyzers only activate when needed
- Event subscriptions determine analyzer participation
- Minimal overhead for unused analysis capabilities

### 6.2 Memory Efficiency
- Shared result storage in arena
- Direct pointer access prevents copying
- Automated cleanup via Fortran intrinsic finalizers

### 6.3 Incremental Analysis
- Framework supports incremental updates
- Analyzers can register dirty node detection
- Future optimization for large codebases

## 7. Future Extensibility

### 7.1 External Plugin Support
- Shared library loading for custom analyzers
- C interop for analyzers in other languages
- Plugin manifest and capability discovery

### 7.2 Analysis Pipelines
- Multiple analysis passes with dependencies
- Parallel analysis for independent analyzers
- Analysis result version tracking

### 7.3 Integration Points
- **Issue #193**: Control flow analysis via plugin
- **Issue #194**: Source reconstruction via specialized analyzer
- **Issue #191**: Enhanced symbol tracking
- **Issue #192**: Advanced scope analysis

## 8. Testing Strategy

### 8.1 Unit Testing
- Each analyzer component independently testable
- Mock event system for isolated testing
- Query engine with test fixtures

### 8.2 Integration Testing  
- Full pipeline with multiple analyzers
- Backward compatibility verification
- Performance regression testing

### 8.3 Plugin Testing
- Template-based plugin validation
- Custom analyzer integration tests
- Memory leak detection across plugin boundaries

## Implementation Notes

This design maintains complete backward compatibility while providing a foundation for unlimited extensibility. The event-driven architecture ensures clean separation of concerns, and the query engine solves the memory issues identified in #196.

The modular design allows incremental implementation without disrupting existing functionality, making it suitable for production deployment throughout the development process.