module builtin_analyzer_factories
    use analyzer_factory
    use base_analyzer, only: base_analyzer_t
    ! Import all built-in analyzer types
    use builtin_analyzers, only: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    use call_graph_analyzer, only: call_graph_analyzer_t  
    use control_flow_analyzer, only: control_flow_analyzer_t
    use usage_tracker_analyzer, only: usage_tracker_analyzer_t
    use source_reconstruction_analyzer, only: source_reconstruction_analyzer_t
    use interface_analyzer, only: interface_analyzer_t
    use mock_analyzers, only: simple_test_analyzer_t
    implicit none
    private

    public :: register_builtin_factories, &
              create_symbol_analyzer, create_type_analyzer, create_scope_analyzer, &
              create_call_graph_analyzer, create_control_flow_analyzer, &
              create_usage_tracker_analyzer, create_source_reconstruction_analyzer, &
              create_interface_analyzer, create_simple_test_analyzer

contains

    ! Factory functions for each analyzer type
    function create_symbol_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(symbol_analyzer_t :: analyzer)
    end function create_symbol_analyzer

    function create_type_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(type_analyzer_t :: analyzer)
    end function create_type_analyzer

    function create_scope_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(scope_analyzer_t :: analyzer)
    end function create_scope_analyzer

    function create_call_graph_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(call_graph_analyzer_t :: analyzer)
    end function create_call_graph_analyzer

    function create_control_flow_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(control_flow_analyzer_t :: analyzer)
    end function create_control_flow_analyzer

    function create_usage_tracker_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(usage_tracker_analyzer_t :: analyzer)
    end function create_usage_tracker_analyzer

    function create_source_reconstruction_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(source_reconstruction_analyzer_t :: analyzer)
    end function create_source_reconstruction_analyzer

    function create_interface_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(interface_analyzer_t :: analyzer)
    end function create_interface_analyzer

    function create_simple_test_analyzer() result(analyzer)
        class(base_analyzer_t), allocatable :: analyzer
        allocate(simple_test_analyzer_t :: analyzer)
    end function create_simple_test_analyzer

    ! Register all built-in analyzer factories
    subroutine register_builtin_factories(registry)
        type(analyzer_registry_t), intent(inout) :: registry
        type(analyzer_factory_t) :: factory

        ! Symbol Analyzer
        factory%creator => create_symbol_analyzer
        factory%type_name = "symbol_analyzer_t"
        factory%description = "Symbol analysis and tracking"
        call registry%register_factory("symbol", factory)

        ! Type Analyzer  
        factory%creator => create_type_analyzer
        factory%type_name = "type_analyzer_t"
        factory%description = "Type analysis and inference"
        call registry%register_factory("type", factory)

        ! Scope Analyzer
        factory%creator => create_scope_analyzer
        factory%type_name = "scope_analyzer_t"
        factory%description = "Scope and variable analysis"
        call registry%register_factory("scope", factory)

        ! Call Graph Analyzer
        factory%creator => create_call_graph_analyzer
        factory%type_name = "call_graph_analyzer_t"
        factory%description = "Function call graph analysis"
        call registry%register_factory("call_graph", factory)

        ! Control Flow Analyzer
        factory%creator => create_control_flow_analyzer
        factory%type_name = "control_flow_analyzer_t"
        factory%description = "Control flow analysis"
        call registry%register_factory("control_flow", factory)

        ! Usage Tracker Analyzer
        factory%creator => create_usage_tracker_analyzer
        factory%type_name = "usage_tracker_analyzer_t"
        factory%description = "Variable usage tracking"
        call registry%register_factory("usage_tracker", factory)

        ! Source Reconstruction Analyzer
        factory%creator => create_source_reconstruction_analyzer
        factory%type_name = "source_reconstruction_analyzer_t"
        factory%description = "Source code reconstruction"
        call registry%register_factory("source_reconstruction", factory)

        ! Interface Analyzer
        factory%creator => create_interface_analyzer
        factory%type_name = "interface_analyzer_t"
        factory%description = "Interface and procedure analysis"
        call registry%register_factory("interface", factory)

        ! Simple Test Analyzer (for testing)
        factory%creator => create_simple_test_analyzer
        factory%type_name = "simple_test_analyzer_t"
        factory%description = "Simple analyzer for testing"
        call registry%register_factory("simple_test", factory)

    end subroutine register_builtin_factories

end module builtin_analyzer_factories