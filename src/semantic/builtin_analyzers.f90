module builtin_analyzers
    use semantic_analyzer_base, only: semantic_analyzer_t
    use ast_core, only: ast_arena_t
    use semantic_analyzer, only: semantic_context_t, create_semantic_context, &
                                 analyze_program
    use type_system_hm, only: mono_type_t
    implicit none
    private

    public :: symbol_analyzer_t, type_analyzer_t, scope_analyzer_t

    ! Symbol collection analyzer - extracts symbols from AST
    type, extends(semantic_analyzer_t) :: symbol_analyzer_t
        type(semantic_context_t) :: context
    contains
        procedure :: analyze => analyze_symbols
        procedure :: get_results => get_symbol_results
        procedure :: get_name => get_symbol_analyzer_name
    end type

    ! Type inference analyzer - performs Hindley-Milner type inference  
    type, extends(semantic_analyzer_t) :: type_analyzer_t
        type(semantic_context_t) :: context
    contains
        procedure :: analyze => analyze_types
        procedure :: get_results => get_type_results  
        procedure :: get_name => get_type_analyzer_name
    end type

    ! Scope management analyzer - builds scope hierarchy
    type, extends(semantic_analyzer_t) :: scope_analyzer_t
        type(semantic_context_t) :: context
    contains
        procedure :: analyze => analyze_scopes
        procedure :: get_results => get_scope_results
        procedure :: get_name => get_scope_analyzer_name
    end type

contains

    ! Symbol analyzer implementation
    subroutine analyze_symbols(this, shared_context, arena, node_index)
        class(symbol_analyzer_t), intent(inout) :: this
        class(*), intent(in) :: shared_context
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
        class(*), allocatable :: results
        
        ! Return the semantic context containing symbols
        allocate(semantic_context_t :: results)
        select type(results)
        type is (semantic_context_t)
            results = this%context
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
        class(*), intent(in) :: shared_context
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
        class(*), allocatable :: results
        
        allocate(semantic_context_t :: results)
        select type(results)
        type is (semantic_context_t)
            results = this%context
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
        class(*), intent(in) :: shared_context
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
        class(*), allocatable :: results
        
        allocate(semantic_context_t :: results)
        select type(results)
        type is (semantic_context_t)
            results = this%context
        end select
    end function

    function get_scope_analyzer_name(this) result(name)
        class(scope_analyzer_t), intent(in) :: this
        character(:), allocatable :: name
        
        name = "scope_analyzer"
    end function

end module builtin_analyzers