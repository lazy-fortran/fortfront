module builtin_analyzers
    use semantic_analyzer_base, only: semantic_analyzer_t
    use semantic_context_types, only: semantic_context_base_t
    use semantic_result_types, only: semantic_result_base_t, &
                                     symbol_result_t, type_result_t, scope_result_t
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use type_system_unified, only: mono_type_t
    
    ! Import analysis plugins
    use call_graph_analyzer, only: call_graph_analyzer_t
    use control_flow_analyzer, only: control_flow_analyzer_t
    use usage_tracker_analyzer, only: usage_tracker_analyzer_t
    use source_reconstruction_analyzer, only: source_reconstruction_analyzer_t
    use interface_analyzer, only: interface_analyzer_t
    use error_handling, only: result_t
    use iso_fortran_env, only: error_unit
    implicit none
    private

    ! Core semantic analyzers (essential for standardization)
    public :: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t
    
    ! Analysis plugins (for external tools like fluff)
    public :: call_graph_analyzer_t, control_flow_analyzer_t
    public :: usage_tracker_analyzer_t, source_reconstruction_analyzer_t
    public :: interface_analyzer_t

    ! Symbol collection analyzer - extracts symbols from AST
    type, extends(semantic_analyzer_t) :: symbol_analyzer_t
        type(semantic_context_t) :: context
    contains
        procedure :: analyze => analyze_symbols
        procedure :: get_results => get_symbol_results
        procedure :: get_name => get_symbol_analyzer_name
        procedure :: assign => assign_symbol_analyzer
        procedure :: get_dependencies => get_symbol_dependencies
    end type

    ! Type inference analyzer - performs Hindley-Milner type inference  
    type, extends(semantic_analyzer_t) :: type_analyzer_t
        type(semantic_context_t) :: context
    contains
        procedure :: analyze => analyze_types
        procedure :: get_results => get_type_results  
        procedure :: get_name => get_type_analyzer_name
        procedure :: assign => assign_type_analyzer
        procedure :: get_dependencies => get_type_dependencies
    end type

    ! Scope management analyzer - builds scope hierarchy
    type, extends(semantic_analyzer_t) :: scope_analyzer_t
        type(semantic_context_t) :: context
    contains
        procedure :: analyze => analyze_scopes
        procedure :: get_results => get_scope_results
        procedure :: get_name => get_scope_analyzer_name
        procedure :: assign => assign_scope_analyzer
        procedure :: get_dependencies => get_scope_dependencies
    end type

contains

    ! Symbol analyzer implementation
    subroutine analyze_symbols(this, shared_context, arena, node_index)
        class(symbol_analyzer_t), intent(inout) :: this
        class(semantic_context_base_t), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Initialize fresh semantic context for symbol collection
        this%context = create_semantic_context()
        
        ! Create a mutable copy of arena for analysis
        ! Note: In production, we'd avoid this copy and modify analyze_program
        ! to accept intent(in) arena, but for now we work with existing API
        block
            type(ast_arena_t) :: mutable_arena
            mutable_arena = arena
            call analyze_program(this%context, mutable_arena, node_index)
        end block
    end subroutine

    function get_symbol_results(this) result(results)
        class(symbol_analyzer_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        
        ! Return the symbol analysis results
        allocate(symbol_result_t :: results)
        select type(results)
        type is (symbol_result_t)
            results%symbols_found = 0  ! Could be populated from context analysis
            results%symbols_resolved = 0
            results%unresolved_symbols = 0
        end select
    end function

    function get_symbol_analyzer_name(this) result(name)
        class(symbol_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "symbol_analyzer"
    end function

    ! Type analyzer implementation
    subroutine analyze_types(this, shared_context, arena, node_index)
        class(type_analyzer_t), intent(inout) :: this
        class(semantic_context_base_t), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Initialize context
        this%context = create_semantic_context()
        
        ! Create mutable copy for analysis
        block
            type(ast_arena_t) :: mutable_arena
            mutable_arena = arena
            call analyze_program(this%context, mutable_arena, node_index)
        end block
    end subroutine

    function get_type_results(this) result(results)
        class(type_analyzer_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        
        allocate(type_result_t :: results)
        select type(results)
        type is (type_result_t)
            results%types_analyzed = 0  ! Could be populated from context analysis
            results%type_errors = 0
            results%type_warnings = 0
            results%type_inference_used = .false.
        end select
    end function

    function get_type_analyzer_name(this) result(name)
        class(type_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "type_analyzer"
    end function

    ! Scope analyzer implementation  
    subroutine analyze_scopes(this, shared_context, arena, node_index)
        class(scope_analyzer_t), intent(inout) :: this
        class(semantic_context_base_t), intent(in) :: shared_context
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        
        ! Initialize context
        this%context = create_semantic_context()
        
        ! Create mutable copy for analysis
        block
            type(ast_arena_t) :: mutable_arena
            mutable_arena = arena
            call analyze_program(this%context, mutable_arena, node_index)
        end block
    end subroutine

    function get_scope_results(this) result(results)
        class(scope_analyzer_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: results
        
        allocate(scope_result_t :: results)
        select type(results)
        type is (scope_result_t)
            results%scopes_analyzed = 0  ! Could be populated from context analysis
            results%scope_violations = 0
        end select
    end function

    function get_scope_analyzer_name(this) result(name)
        class(scope_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "scope_analyzer"
    end function

    ! Assignment operators for deep copy
    subroutine assign_symbol_analyzer(lhs, rhs)
        class(symbol_analyzer_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (symbol_analyzer_t)
            lhs%context = rhs%context
        class default
            write(error_unit, '(A)') "ERROR [builtin_analyzers]: Type mismatch in symbol_analyzer assignment - assignment ignored"
            ! Don't perform assignment on type mismatch
        end select
    end subroutine

    subroutine assign_type_analyzer(lhs, rhs)
        class(type_analyzer_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (type_analyzer_t)
            lhs%context = rhs%context
        class default
            write(error_unit, '(A)') "ERROR [builtin_analyzers]: Type mismatch in type_analyzer assignment - assignment ignored"
            ! Don't perform assignment on type mismatch
        end select
    end subroutine

    subroutine assign_scope_analyzer(lhs, rhs)
        class(scope_analyzer_t), intent(out) :: lhs
        class(semantic_analyzer_t), intent(in) :: rhs
        
        select type(rhs)
        type is (scope_analyzer_t)
            lhs%context = rhs%context
        class default
            write(error_unit, '(A)') "ERROR [builtin_analyzers]: Type mismatch in scope_analyzer assignment - assignment ignored"
            ! Don't perform assignment on type mismatch
        end select
    end subroutine

    ! Dependency functions for builtin analyzers
    function get_symbol_dependencies(this) result(deps)
        class(symbol_analyzer_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        
        ! Symbol analyzer has no dependencies - runs first
        allocate(character(len=0) :: deps(0))
        
        associate(dummy => this)
        end associate
    end function

    function get_type_dependencies(this) result(deps)
        class(type_analyzer_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        
        ! Type analyzer depends on symbols
        allocate(character(len=16) :: deps(1))
        deps(1) = "symbol_analyzer"
        
        associate(dummy => this)
        end associate
    end function

    function get_scope_dependencies(this) result(deps)
        class(scope_analyzer_t), intent(in) :: this
        character(:), allocatable :: deps(:)
        
        ! Scope analyzer has no dependencies for now
        allocate(character(len=0) :: deps(0))
        
        associate(dummy => this)
        end associate
    end function

end module builtin_analyzers