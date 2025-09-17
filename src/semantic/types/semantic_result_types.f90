module semantic_result_types
    ! Define concrete result types to replace class(*) in semantic analysis results
    implicit none
    private

    ! Base result type for all semantic analysis results
    type, abstract, public :: semantic_result_base_t
        integer :: result_id = 0
        character(:), allocatable :: result_type_name
        logical :: has_errors = .false.
        logical :: has_warnings = .false.
        character(:), allocatable :: summary
    contains
        procedure(get_result_type_interface), deferred :: get_result_type
        procedure(clone_result_interface), deferred :: clone_result
        procedure(merge_results_interface), deferred :: merge_results
    end type semantic_result_base_t

    ! Symbol analysis results
    type, extends(semantic_result_base_t), public :: symbol_result_t
        integer :: symbols_found = 0
        integer :: symbols_resolved = 0
        integer :: unresolved_symbols = 0
        character(:), allocatable :: symbol_table_summary
    contains
        procedure :: get_result_type => symbol_get_result_type
        procedure :: clone_result => symbol_clone_result
        procedure :: merge_results => symbol_merge_results
        procedure :: assign => symbol_result_assign
        generic :: assignment(=) => assign
    end type symbol_result_t

    ! Type analysis results
    type, extends(semantic_result_base_t), public :: type_result_t
        integer :: types_analyzed = 0
        integer :: type_errors = 0
        integer :: type_warnings = 0
        logical :: type_inference_used = .false.
    contains
        procedure :: get_result_type => type_get_result_type
        procedure :: clone_result => type_clone_result
        procedure :: merge_results => type_merge_results
        procedure :: assign => type_result_assign
        generic :: assignment(=) => assign
    end type type_result_t

    ! Scope analysis results
    type, extends(semantic_result_base_t), public :: scope_result_t
        integer :: scopes_analyzed = 0
        integer :: scope_violations = 0
        character(:), allocatable :: scope_tree_summary
    contains
        procedure :: get_result_type => scope_get_result_type
        procedure :: clone_result => scope_clone_result
        procedure :: merge_results => scope_merge_results
        procedure :: assign => scope_result_assign
        generic :: assignment(=) => assign
    end type scope_result_t

    ! Usage tracking results
    type, extends(semantic_result_base_t), public :: usage_result_t
        integer :: variables_tracked = 0
        integer :: unused_variables = 0
        integer :: undefined_variables = 0
    contains
        procedure :: get_result_type => usage_get_result_type
        procedure :: clone_result => usage_clone_result
        procedure :: merge_results => usage_merge_results
        procedure :: assign => usage_result_assign
        generic :: assignment(=) => assign
    end type usage_result_t

    ! Control flow analysis results
    type, extends(semantic_result_base_t), public :: control_flow_result_t
        integer :: basic_blocks = 0
        integer :: unreachable_blocks = 0
        logical :: has_infinite_loops = .false.
    contains
        procedure :: get_result_type => control_flow_get_result_type
        procedure :: clone_result => control_flow_clone_result
        procedure :: merge_results => control_flow_merge_results
        procedure :: assign => control_flow_result_assign
        generic :: assignment(=) => assign
    end type control_flow_result_t

    ! Call graph analysis results
    type, extends(semantic_result_base_t), public :: call_graph_result_t
        integer :: procedures_found = 0
        integer :: call_relationships = 0
        logical :: has_recursive_calls = .false.
    contains
        procedure :: get_result_type => call_graph_get_result_type
        procedure :: clone_result => call_graph_clone_result
        procedure :: merge_results => call_graph_merge_results
        procedure :: assign => call_graph_result_assign
        generic :: assignment(=) => assign
    end type call_graph_result_t

    ! Simple test results for testing purposes
    type, extends(semantic_result_base_t), public :: test_result_t
        logical :: analysis_executed = .false.
        integer :: nodes_visited = 0
    contains
        procedure :: get_result_type => test_get_result_type
        procedure :: clone_result => test_clone_result
        procedure :: merge_results => test_merge_results
        procedure :: assign => test_result_assign
        generic :: assignment(=) => assign
    end type test_result_t

    ! Abstract interfaces
    abstract interface
        function get_result_type_interface(this) result(type_name)
            import :: semantic_result_base_t
            class(semantic_result_base_t), intent(in) :: this
            character(:), allocatable :: type_name
        end function get_result_type_interface

        function clone_result_interface(this) result(cloned)
            import :: semantic_result_base_t
            class(semantic_result_base_t), intent(in) :: this
            class(semantic_result_base_t), allocatable :: cloned
        end function clone_result_interface

        subroutine merge_results_interface(this, other)
            import :: semantic_result_base_t
            class(semantic_result_base_t), intent(inout) :: this
            class(semantic_result_base_t), intent(in) :: other
        end subroutine merge_results_interface
    end interface

    ! Types are already declared public above

contains

    ! Symbol result implementations
    function symbol_get_result_type(this) result(type_name)
        class(symbol_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "symbol_result"
    end function symbol_get_result_type

    function symbol_clone_result(this) result(cloned)
        class(symbol_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(symbol_result_t) :: temp_result
        
        temp_result = this  ! Use assignment operator
        allocate(cloned, source=temp_result)
    end function symbol_clone_result

    subroutine symbol_merge_results(this, other)
        class(symbol_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (symbol_result_t)
            this%symbols_found = this%symbols_found + other_result%symbols_found
            this%symbols_resolved = this%symbols_resolved + &
                                  other_result%symbols_resolved
            this%unresolved_symbols = this%unresolved_symbols + &
                                     other_result%unresolved_symbols
            this%has_errors = this%has_errors .or. other_result%has_errors
            this%has_warnings = this%has_warnings .or. other_result%has_warnings
        end select
    end subroutine symbol_merge_results

    subroutine symbol_result_assign(lhs, rhs)
        class(symbol_result_t), intent(out) :: lhs
        type(symbol_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%symbols_found = rhs%symbols_found
        lhs%symbols_resolved = rhs%symbols_resolved
        lhs%unresolved_symbols = rhs%unresolved_symbols
        lhs%symbol_table_summary = rhs%symbol_table_summary
    end subroutine symbol_result_assign

    ! Type result implementations  
    function type_get_result_type(this) result(type_name)
        class(type_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "type_result"
    end function type_get_result_type

    function type_clone_result(this) result(cloned)
        class(type_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(type_result_t) :: temp_result
        
        temp_result = this
        allocate(cloned, source=temp_result)
    end function type_clone_result

    subroutine type_merge_results(this, other)
        class(type_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (type_result_t)
            this%types_analyzed = this%types_analyzed + &
                                 other_result%types_analyzed
            this%type_errors = this%type_errors + other_result%type_errors
            this%type_warnings = this%type_warnings + other_result%type_warnings
            this%type_inference_used = this%type_inference_used .or. &
                                      other_result%type_inference_used
        end select
    end subroutine type_merge_results

    subroutine type_result_assign(lhs, rhs)
        class(type_result_t), intent(out) :: lhs
        type(type_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%types_analyzed = rhs%types_analyzed
        lhs%type_errors = rhs%type_errors
        lhs%type_warnings = rhs%type_warnings
        lhs%type_inference_used = rhs%type_inference_used
    end subroutine type_result_assign

    ! Scope result implementations
    function scope_get_result_type(this) result(type_name)
        class(scope_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "scope_result"
    end function scope_get_result_type

    function scope_clone_result(this) result(cloned)
        class(scope_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(scope_result_t) :: temp_result
        
        temp_result = this
        allocate(cloned, source=temp_result)
    end function scope_clone_result

    subroutine scope_merge_results(this, other)
        class(scope_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (scope_result_t)
            this%scopes_analyzed = this%scopes_analyzed + &
                                  other_result%scopes_analyzed
            this%scope_violations = this%scope_violations + &
                                   other_result%scope_violations
        end select
    end subroutine scope_merge_results

    subroutine scope_result_assign(lhs, rhs)
        class(scope_result_t), intent(out) :: lhs
        type(scope_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%scopes_analyzed = rhs%scopes_analyzed
        lhs%scope_violations = rhs%scope_violations
        lhs%scope_tree_summary = rhs%scope_tree_summary
    end subroutine scope_result_assign

    ! Usage result implementations
    function usage_get_result_type(this) result(type_name)
        class(usage_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "usage_result"
    end function usage_get_result_type

    function usage_clone_result(this) result(cloned)
        class(usage_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(usage_result_t) :: temp_result
        
        temp_result = this
        allocate(cloned, source=temp_result)
    end function usage_clone_result

    subroutine usage_merge_results(this, other)
        class(usage_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (usage_result_t)
            this%variables_tracked = this%variables_tracked + &
                                    other_result%variables_tracked
            this%unused_variables = this%unused_variables + &
                                   other_result%unused_variables
            this%undefined_variables = this%undefined_variables + &
                                      other_result%undefined_variables
        end select
    end subroutine usage_merge_results

    subroutine usage_result_assign(lhs, rhs)
        class(usage_result_t), intent(out) :: lhs
        type(usage_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%variables_tracked = rhs%variables_tracked
        lhs%unused_variables = rhs%unused_variables
        lhs%undefined_variables = rhs%undefined_variables
    end subroutine usage_result_assign

    ! Control flow result implementations
    function control_flow_get_result_type(this) result(type_name)
        class(control_flow_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "control_flow_result"
    end function control_flow_get_result_type

    function control_flow_clone_result(this) result(cloned)
        class(control_flow_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(control_flow_result_t) :: temp_result
        
        temp_result = this
        allocate(cloned, source=temp_result)
    end function control_flow_clone_result

    subroutine control_flow_merge_results(this, other)
        class(control_flow_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (control_flow_result_t)
            this%basic_blocks = this%basic_blocks + other_result%basic_blocks
            this%unreachable_blocks = this%unreachable_blocks + &
                                     other_result%unreachable_blocks
            this%has_infinite_loops = this%has_infinite_loops .or. &
                                     other_result%has_infinite_loops
        end select
    end subroutine control_flow_merge_results

    subroutine control_flow_result_assign(lhs, rhs)
        class(control_flow_result_t), intent(out) :: lhs
        type(control_flow_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%basic_blocks = rhs%basic_blocks
        lhs%unreachable_blocks = rhs%unreachable_blocks
        lhs%has_infinite_loops = rhs%has_infinite_loops
    end subroutine control_flow_result_assign

    ! Call graph result implementations
    function call_graph_get_result_type(this) result(type_name)
        class(call_graph_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        type_name = "call_graph_result"
    end function call_graph_get_result_type

    function call_graph_clone_result(this) result(cloned)
        class(call_graph_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        type(call_graph_result_t) :: temp_result
        
        temp_result = this
        allocate(cloned, source=temp_result)
    end function call_graph_clone_result

    subroutine call_graph_merge_results(this, other)
        class(call_graph_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type (other_result => other)
        type is (call_graph_result_t)
            this%procedures_found = this%procedures_found + &
                                   other_result%procedures_found
            this%call_relationships = this%call_relationships + &
                                     other_result%call_relationships
            this%has_recursive_calls = this%has_recursive_calls .or. &
                                      other_result%has_recursive_calls
        end select
    end subroutine call_graph_merge_results

    subroutine call_graph_result_assign(lhs, rhs)
        class(call_graph_result_t), intent(out) :: lhs
        type(call_graph_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%procedures_found = rhs%procedures_found
        lhs%call_relationships = rhs%call_relationships
        lhs%has_recursive_calls = rhs%has_recursive_calls
    end subroutine call_graph_result_assign

    ! Test result implementations
    function test_get_result_type(this) result(type_name)
        class(test_result_t), intent(in) :: this
        character(:), allocatable :: type_name
        
        type_name = "test_result_t"
        associate(dummy => this)
        end associate
    end function test_get_result_type

    function test_clone_result(this) result(cloned)
        class(test_result_t), intent(in) :: this
        class(semantic_result_base_t), allocatable :: cloned
        
        allocate(test_result_t :: cloned)
        select type(cloned)
        type is (test_result_t)
            cloned = this
        end select
    end function test_clone_result

    subroutine test_merge_results(this, other)
        class(test_result_t), intent(inout) :: this
        class(semantic_result_base_t), intent(in) :: other
        
        select type(other)
        type is (test_result_t)
            this%has_errors = this%has_errors .or. other%has_errors
            this%has_warnings = this%has_warnings .or. other%has_warnings
            this%nodes_visited = this%nodes_visited + other%nodes_visited
        end select
    end subroutine test_merge_results

    subroutine test_result_assign(lhs, rhs)
        class(test_result_t), intent(out) :: lhs
        type(test_result_t), intent(in) :: rhs
        
        lhs%result_id = rhs%result_id
        lhs%result_type_name = rhs%result_type_name
        lhs%has_errors = rhs%has_errors
        lhs%has_warnings = rhs%has_warnings
        lhs%summary = rhs%summary
        lhs%analysis_executed = rhs%analysis_executed
        lhs%nodes_visited = rhs%nodes_visited
    end subroutine test_result_assign

end module semantic_result_types