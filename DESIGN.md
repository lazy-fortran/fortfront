# Core Type Analyzer Plugin Architecture Design

## Architecture Overview

The Core Type Analyzer Plugin Architecture extracts the Hindley-Milner type inference logic from the monolithic `semantic_analyzer.f90` (2155+ lines) into a focused, event-driven plugin system. This design preserves all existing functionality while enabling extensibility for future semantic analysis capabilities.

## Design Principles

- **SOLID Compliance**: Each analyzer has a single responsibility with clean interfaces
- **Event-Driven Architecture**: Plugins communicate through well-defined events
- **Dependency Resolution**: Automatic execution ordering based on plugin dependencies
- **Zero Functionality Loss**: Complete preservation of existing type inference capabilities
- **Performance-Oriented**: Efficient event dispatch and minimal overhead
- **Testable Design**: Each plugin is independently testable with clear contracts

## System Components

### 1. Event System Core

#### Event Types
```fortran
integer, parameter :: EVENT_NODE_ENTER = 1
integer, parameter :: EVENT_NODE_EXIT = 2
integer, parameter :: EVENT_SCOPE_ENTER = 3
integer, parameter :: EVENT_SCOPE_EXIT = 4
integer, parameter :: EVENT_TYPE_INFERRED = 5
integer, parameter :: EVENT_ANALYSIS_COMPLETE = 6
integer, parameter :: EVENT_ERROR_DETECTED = 7
integer, parameter :: EVENT_BUILTIN_REQUIRED = 8
```

#### Event Data Structures
```fortran
type :: analysis_event_t
    integer :: event_type
    integer :: node_index
    integer :: source_analyzer_id
    class(*), allocatable :: event_data
    logical :: consumed = .false.
    logical :: propagate = .true.
end type

type :: event_subscription_t
    integer :: event_type
    integer :: analyzer_id
    procedure(event_handler_interface), pointer :: handler => null()
end type
```

### 2. Core Type Analyzer Plugin

#### Extracted Functionality from semantic_analyzer.f90
- **Type Inference Methods**: `infer_type`, `infer_assignment`, `infer_binary_op`, `infer_function_call`
- **Literal Type Inference**: `infer_literal`, `infer_identifier`
- **Array Operations**: `infer_array_literal`, `infer_array_slice`, `infer_implied_do_loop`
- **Type Unification**: `unify_types`, `occurs_check`
- **Type Generalization**: `generalize_type`, `instantiate_type_scheme`
- **Builtin Functions**: `get_builtin_function_type` management
- **Substitution Management**: Complete substitution composition and application

#### Core Plugin Interface
```fortran
type, extends(base_analyzer_t) :: core_type_analyzer_t
    type(semantic_context_t) :: context
    integer, allocatable :: subscribed_events(:)
contains
    procedure :: analyze => core_type_analyze
    procedure :: handle_node_enter => on_node_enter
    procedure :: handle_node_exit => on_node_exit
    procedure :: handle_scope_change => on_scope_change
    procedure :: infer_node_type => infer_type_for_node
    procedure :: validate_assignment => validate_assignment_types
    procedure :: register_builtin => register_builtin_function
end type
```

### 3. Analysis Orchestrator

#### Plugin Registration and Management
```fortran
type :: analysis_orchestrator_t
    type(plugin_registry_t) :: registry
    type(event_dispatcher_t) :: dispatcher
    type(dependency_resolver_t) :: resolver
    type(analysis_cache_t) :: cache
    type(semantic_context_t) :: shared_context
    logical :: events_enabled = .true.
contains
    procedure :: register_plugin => orchestrator_register_plugin
    procedure :: execute_analysis => orchestrator_execute_analysis
    procedure :: dispatch_event => orchestrator_dispatch_event
    procedure :: resolve_execution_order => orchestrator_resolve_order
    procedure :: validate_plugin_compatibility => validate_compatibility
end type
```

#### Plugin Lifecycle Management
1. **Registration Phase**: Plugin registration with dependency declaration
2. **Validation Phase**: Dependency resolution and compatibility checking
3. **Initialization Phase**: Plugin initialization with shared context
4. **Analysis Phase**: Event-driven execution with proper ordering
5. **Cleanup Phase**: Resource deallocation and result aggregation

### 4. Enhanced Base Analyzer

#### Extended Base Class
```fortran
type, abstract, extends(base_analyzer_t) :: event_aware_analyzer_t
    integer, allocatable :: subscribed_events(:)
    logical :: event_processing_enabled = .true.
    real :: event_processing_priority = 1.0
contains
    procedure(event_handler_interface), deferred :: handle_event
    procedure :: subscribe_to_event => analyzer_subscribe_event
    procedure :: unsubscribe_from_event => analyzer_unsubscribe_event
    procedure :: emit_event => analyzer_emit_event
    procedure :: get_event_subscriptions => analyzer_get_subscriptions
end type
```

## File Structure and Module Organization

### Core Event System
```
src/semantic/
├── events/
│   ├── analysis_events.f90          # Event type definitions and data structures
│   ├── event_dispatcher.f90         # Central event routing and dispatch
│   ├── event_subscription.f90       # Subscription management
│   └── event_handlers.f90           # Common event handler interfaces
```

### Plugin Infrastructure
```
src/semantic/
├── plugins/
│   ├── base_plugin_analyzer.f90     # Enhanced base class with events
│   ├── plugin_registry.f90          # Plugin registration and discovery
│   ├── dependency_resolver.f90      # Plugin dependency resolution
│   └── plugin_lifecycle.f90         # Plugin initialization and cleanup
```

### Core Type Analyzer Plugin
```
src/semantic/
├── plugins/
│   ├── core_type_analyzer/
│   │   ├── core_type_analyzer.f90   # Main plugin implementation
│   │   ├── type_inference_engine.f90 # Extracted HM inference logic
│   │   ├── builtin_type_manager.f90  # Builtin function type management
│   │   ├── assignment_validator.f90  # Assignment type validation
│   │   └── array_type_handler.f90    # Array-specific type operations
```

### Analysis Orchestration
```
src/semantic/
├── orchestration/
│   ├── analysis_orchestrator.f90    # Main orchestrator implementation
│   ├── execution_planner.f90        # Analysis execution planning
│   ├── result_aggregator.f90        # Plugin result aggregation
│   └── context_manager.f90          # Shared context management
```

## Event System Architecture

### Event Flow Design
1. **Analysis Start**: Orchestrator emits `EVENT_ANALYSIS_COMPLETE` to initialize plugins
2. **Node Processing**: For each AST node, emit `EVENT_NODE_ENTER`
3. **Type Inference**: Core Type Analyzer processes node and emits `EVENT_TYPE_INFERRED`
4. **Node Completion**: Emit `EVENT_NODE_EXIT` after all plugins process node
5. **Scope Management**: Emit `EVENT_SCOPE_ENTER`/`EVENT_SCOPE_EXIT` for scope changes
6. **Error Handling**: Emit `EVENT_ERROR_DETECTED` for type errors or conflicts

### Event Subscription Model
```fortran
! Plugin registers interest in specific events
call orchestrator%dispatcher%subscribe(EVENT_NODE_ENTER, plugin_id, handler_proc)
call orchestrator%dispatcher%subscribe(EVENT_TYPE_INFERRED, plugin_id, validation_proc)

! Event dispatch with priority ordering
call orchestrator%dispatcher%dispatch(event, subscriber_priorities)
```

### Event Data Payload
```fortran
type :: type_inference_event_data_t
    integer :: node_index
    type(mono_type_t) :: inferred_type
    logical :: inference_successful
    character(len=256) :: error_message
    integer :: confidence_level
end type

type :: scope_event_data_t
    character(len=64) :: scope_name
    integer :: scope_level
    logical :: entering_scope
    integer :: parent_scope_index
end type
```

## Interface Definitions

### Event Handler Interface
```fortran
abstract interface
    subroutine event_handler_interface(this, event, context, arena)
        import :: event_aware_analyzer_t, analysis_event_t, semantic_context_t, ast_arena_t
        class(event_aware_analyzer_t), intent(inout) :: this
        type(analysis_event_t), intent(inout) :: event
        type(semantic_context_t), intent(inout) :: context
        type(ast_arena_t), intent(inout) :: arena
    end subroutine
end interface
```

### Plugin Registration Interface
```fortran
abstract interface
    subroutine plugin_factory_interface(plugin, config)
        import :: event_aware_analyzer_t
        class(event_aware_analyzer_t), allocatable, intent(out) :: plugin
        class(*), intent(in), optional :: config
    end subroutine
end interface
```

### Type Inference Interface
```fortran
abstract interface
    function type_inference_interface(this, context, arena, node_index) result(typ)
        import :: core_type_analyzer_t, semantic_context_t, ast_arena_t, mono_type_t
        class(core_type_analyzer_t), intent(inout) :: this
        type(semantic_context_t), intent(inout) :: context
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: node_index
        type(mono_type_t) :: typ
    end function
end interface
```

## Migration Strategy

### Phase 1: Event System Foundation
1. Implement core event system without breaking existing code
2. Create enhanced base analyzer with event awareness
3. Develop plugin registry and orchestrator skeleton
4. Maintain existing `semantic_analyzer.f90` functionality alongside new system

### Phase 2: Core Type Analyzer Extraction
1. Extract type inference methods from `semantic_analyzer.f90` into plugin
2. Implement event-driven communication between type analyzer and orchestrator
3. Create comprehensive test suite covering all extracted functionality
4. Validate that plugin produces identical results to monolithic implementation

### Phase 3: Integration and Optimization
1. Replace calls to monolithic analyzer with orchestrator
2. Implement dependency resolution for existing analyzer plugins
3. Optimize event dispatch performance and memory usage
4. Remove deprecated code paths from `semantic_analyzer.f90`

### Phase 4: Extension and Documentation
1. Implement additional analyzer plugins (Issues #191-194)
2. Create plugin development documentation and examples
3. Performance benchmarking and optimization
4. Production deployment validation

## Test Architecture

### Unit Testing Strategy
```fortran
! Each plugin component has isolated unit tests
test/semantic/plugins/
├── test_core_type_analyzer.f90      # Core type inference tests
├── test_event_dispatcher.f90        # Event system tests
├── test_plugin_registry.f90         # Plugin registration tests
├── test_dependency_resolver.f90     # Dependency resolution tests
└── test_analysis_orchestrator.f90   # Integration tests
```

### Integration Testing Approach
- **Regression Tests**: Validate identical behavior to monolithic analyzer
- **Event Flow Tests**: Verify proper event dispatch and handling
- **Dependency Tests**: Validate plugin execution ordering
- **Performance Tests**: Ensure no significant performance degradation
- **Error Handling Tests**: Validate robust error propagation and recovery

### Test Coverage Requirements
- **Line Coverage**: Minimum 95% for all plugin components
- **Branch Coverage**: Minimum 90% for complex type inference logic
- **Integration Coverage**: 100% coverage of existing semantic analyzer functionality
- **Event Coverage**: All event types must have comprehensive test scenarios

## Integration Points

### Existing Codebase Integration
1. **AST Arena Compatibility**: Full compatibility with existing arena-based AST storage
2. **Type System Integration**: Seamless integration with `type_system_hm.f90`
3. **Scope Manager Compatibility**: Preservation of existing scope management
4. **Frontend Integration**: Transparent integration with `frontend.f90` compilation pipeline

### Performance Considerations
- **Event Dispatch Overhead**: < 5% performance impact for event-driven architecture
- **Memory Usage**: Efficient event queue management with bounded memory growth
- **Plugin Loading**: Lazy plugin initialization to minimize startup overhead
- **Type Inference Cache**: Preservation of existing type inference caching mechanisms

## Error Handling and Validation

### Plugin Validation
- **Interface Compliance**: Automatic validation of plugin interface implementation
- **Dependency Validation**: Circular dependency detection and resolution
- **Event Contract Validation**: Verification of proper event handling contracts
- **Type Safety**: Compile-time verification of type inference correctness

### Error Recovery
- **Plugin Failure Isolation**: Individual plugin failures do not compromise entire analysis
- **Graceful Degradation**: Fallback to reduced functionality when plugins fail
- **Error Propagation**: Clear error reporting through event system
- **Recovery Mechanisms**: Automatic retry and alternative plugin selection

## Future Extensibility

### Plugin Development Framework
- **Plugin Templates**: Standard templates for common analyzer patterns
- **Development Tools**: Code generation tools for plugin boilerplate
- **Testing Framework**: Standardized testing utilities for plugin development
- **Documentation Generator**: Automatic documentation generation from plugin interfaces

### Advanced Features
- **Plugin Hot-Reloading**: Dynamic plugin loading without restart (future consideration)
- **Distributed Analysis**: Plugin execution across multiple processes (future consideration)
- **Custom Event Types**: Support for user-defined event types and handlers
- **Plugin Marketplace**: Registry system for third-party plugin distribution

## Conclusion

This Core Type Analyzer Plugin Architecture provides a robust, extensible foundation for semantic analysis while preserving all existing functionality. The event-driven design enables clean separation of concerns and facilitates future enhancement without architectural disruption.

The architecture serves as the foundation for Issues #191-194, providing the plugin infrastructure necessary for implementing specialized analyzers for control flow, usage tracking, and source reconstruction. The modular design ensures that each analyzer can be developed, tested, and maintained independently while participating in a coordinated analysis pipeline.

Key benefits of this architecture:
- **Maintainability**: Clear separation of concerns and single responsibility principle
- **Testability**: Independent testing of each component with comprehensive coverage
- **Extensibility**: Easy addition of new analyzer plugins without core changes
- **Performance**: Efficient event dispatch with minimal overhead
- **Reliability**: Robust error handling and plugin failure isolation