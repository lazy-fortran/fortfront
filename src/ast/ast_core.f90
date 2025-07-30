module ast_core
    use json_module
    use type_system_hm, only: mono_type_t
    use string_types, only: string_t
    implicit none
    private

    ! Base AST node type used by all dialects
    type, abstract, public :: ast_node
        integer :: line = 1
        integer :: column = 1
        type(mono_type_t), allocatable :: inferred_type  ! Type information from semantic analysis
    contains
        procedure(visit_interface), deferred :: accept
        procedure(to_json_interface), deferred :: to_json
    end type ast_node

    ! Stack entry for AST nodes
    type :: ast_entry_t
        class(ast_node), allocatable :: node    ! The AST node itself
        integer :: parent_index = 0             ! Index of parent node in stack (0 for root)
        integer :: depth = 0                    ! Depth in tree (0 for root)
        character(len=:), allocatable :: node_type  ! Type name for debugging
        integer, allocatable :: child_indices(:)    ! Indices of child nodes
        integer :: child_count = 0              ! Number of children
    contains
        procedure :: deep_copy => ast_entry_deep_copy
        procedure :: assign => ast_entry_assign
        generic :: assignment(=) => assign
    end type ast_entry_t

    ! High-performance arena-based AST storage system
    type, public :: ast_arena_t
        type(ast_entry_t), allocatable :: entries(:)  ! Contiguous array of entries
        integer :: size = 0                           ! Current number of entries
        integer :: capacity = 0                       ! Array capacity
        integer :: current_index = 0                  ! Current position in arena
        integer :: max_depth = 0                      ! Maximum depth reached
        integer :: chunk_size = 1024                  ! Default chunk size for growth
        integer :: initial_capacity = 256             ! Starting capacity
    contains
        procedure :: push => ast_arena_push
        procedure :: pop => ast_arena_pop
        procedure :: current => ast_arena_current
        procedure :: get_parent => ast_arena_get_parent
        procedure :: get_depth => ast_arena_get_depth
        procedure :: traverse_depth => ast_arena_traverse_depth
        procedure :: find_by_type => ast_arena_find_by_type
        procedure :: get_children => ast_arena_get_children
        procedure :: get_stats => ast_arena_get_stats
        procedure :: clear => ast_arena_clear
        procedure :: add_child => ast_arena_add_child
        procedure :: shrink_arena
        procedure :: deep_copy => ast_arena_deep_copy
        procedure :: assign => ast_arena_assign
        generic :: assignment(=) => assign
    end type ast_arena_t

    ! Abstract interfaces for visitor pattern and JSON serialization
    abstract interface
        subroutine visit_interface(this, visitor)
            import :: ast_node
            class(ast_node), intent(in) :: this
            class(*), intent(inout) :: visitor
        end subroutine visit_interface

        subroutine to_json_interface(this, json, parent)
            use json_module
            import :: ast_node
            class(ast_node), intent(in) :: this
            type(json_core), intent(inout) :: json
            type(json_value), pointer, intent(in) :: parent
        end subroutine to_json_interface
    end interface

    ! Statistics for performance monitoring
    type, public :: ast_arena_stats_t
        integer :: total_nodes = 0
        integer :: max_depth = 0
        integer :: capacity = 0
        integer :: memory_usage = 0  ! Approximate memory usage in bytes
    end type ast_arena_stats_t

    ! Core AST node types shared by all Fortran dialects
    ! KEEP OLD STRUCTURE FOR COMPATIBILITY but also support stack-based access

    ! Program node
    type, extends(ast_node), public :: program_node
        character(len=:), allocatable :: name
        integer, allocatable :: body_indices(:)  ! Indices to body nodes in stack
    contains
        procedure :: accept => program_accept
        procedure :: to_json => program_to_json
    end type program_node

    ! Assignment node
    type, extends(ast_node), public :: assignment_node
        integer :: target_index      ! Index to target node in stack
        integer :: value_index       ! Index to value node in stack
        ! Type inference support (dialect-agnostic)
        logical :: type_was_inferred = .false.  ! true if type was inferred
        character(len=:), allocatable :: inferred_type_name
    contains
        procedure :: accept => assignment_accept
        procedure :: to_json => assignment_to_json
    end type assignment_node

    ! Pointer assignment node (ptr => target)
    type, extends(ast_node), public :: pointer_assignment_node
        integer :: pointer_index     ! Index to pointer node in stack
        integer :: target_index      ! Index to target node in stack
    contains
        procedure :: accept => pointer_assignment_accept
        procedure :: to_json => pointer_assignment_to_json
    end type pointer_assignment_node

    ! Binary operation node
    type, extends(ast_node), public :: binary_op_node
        integer :: left_index        ! Index to left operand in stack
        integer :: right_index       ! Index to right operand in stack
        character(len=:), allocatable :: operator
    contains
        procedure :: accept => binary_op_accept
        procedure :: to_json => binary_op_to_json
    end type binary_op_node

    ! Function definition node
    type, extends(ast_node), public :: function_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        character(len=:), allocatable :: return_type
        integer, allocatable :: body_indices(:)
    contains
        procedure :: accept => function_def_accept
        procedure :: to_json => function_def_to_json
    end type function_def_node

    ! Subroutine definition node
    type, extends(ast_node), public :: subroutine_def_node
        character(len=:), allocatable :: name
        integer, allocatable :: param_indices(:)
        integer, allocatable :: body_indices(:)
    contains
        procedure :: accept => subroutine_def_accept
        procedure :: to_json => subroutine_def_to_json
    end type subroutine_def_node

    ! Subroutine call node (represents explicit CALL statements)
    type, extends(ast_node), public :: subroutine_call_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => subroutine_call_accept
        procedure :: to_json => subroutine_call_to_json
    end type subroutine_call_node

    ! Call or subscript node (represents both function calls and array indexing)
    type, extends(ast_node), public :: call_or_subscript_node
        character(len=:), allocatable :: name
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => call_or_subscript_accept
        procedure :: to_json => call_or_subscript_to_json
        procedure :: assign => call_or_subscript_assign
        generic :: assignment(=) => assign
    end type call_or_subscript_node

    ! Identifier node
    type, extends(ast_node), public :: identifier_node
        character(len=:), allocatable :: name
    contains
        procedure :: accept => identifier_accept
        procedure :: to_json => identifier_to_json
    end type identifier_node

    ! Literal node
    type, extends(ast_node), public :: literal_node
        character(len=:), allocatable :: value
        integer :: literal_kind = 0  ! INTEGER_LITERAL, REAL_LITERAL, etc.
    contains
        procedure :: accept => literal_accept
        procedure :: to_json => literal_to_json
    end type literal_node

    ! Array literal node
    type, extends(ast_node), public :: array_literal_node
        integer, allocatable :: element_indices(:)  ! Indices to element expressions in arena
    contains
        procedure :: accept => array_literal_accept
        procedure :: to_json => array_literal_to_json
        procedure :: assign => array_literal_assign
        generic :: assignment(=) => assign
    end type array_literal_node

    ! Complex literal node
    type, extends(ast_node), public :: complex_literal_node
        integer :: real_index = 0     ! Index to real part expression in arena
        integer :: imag_index = 0     ! Index to imaginary part expression in arena
    contains
        procedure :: accept => complex_literal_accept
        procedure :: to_json => complex_literal_to_json
        procedure :: assign => complex_literal_assign
        generic :: assignment(=) => assign
    end type complex_literal_node

    ! Use statement node
    type, extends(ast_node), public :: use_statement_node
        character(len=:), allocatable :: module_name
        type(string_t), allocatable :: only_list(:)       ! Optional only clause items
        type(string_t), allocatable :: rename_list(:)     ! Optional rename mappings (new_name => old_name)
        logical :: has_only = .false.                     ! Whether the only clause is present
    contains
        procedure :: accept => use_statement_accept
        procedure :: to_json => use_statement_to_json
    end type use_statement_node

    ! Include statement node
    type, extends(ast_node), public :: include_statement_node
        character(len=:), allocatable :: filename
    contains
        procedure :: accept => include_statement_accept
        procedure :: to_json => include_statement_to_json
    end type include_statement_node

    ! Print statement node
    type, extends(ast_node), public :: print_statement_node
        character(len=:), allocatable :: format_spec  ! Optional format
        integer, allocatable :: arg_indices(:)
    contains
        procedure :: accept => print_statement_accept
        procedure :: to_json => print_statement_to_json
    end type print_statement_node

    ! Write statement node
    type, extends(ast_node), public :: write_statement_node
        character(len=:), allocatable :: unit_spec     ! Unit specifier (e.g., "10", "*")
        character(len=:), allocatable :: format_spec   ! Optional format
        integer, allocatable :: arg_indices(:)         ! Arguments to write
        integer :: iostat_var_index = 0                 ! Optional iostat variable index
        integer :: err_label_index = 0                  ! Optional err label index
        integer :: end_label_index = 0                  ! Optional end label index
        integer :: format_expr_index = 0                ! Optional runtime format expression
        logical :: is_formatted = .false.               ! True if formatted I/O
    contains
        procedure :: accept => write_statement_accept
        procedure :: to_json => write_statement_to_json
    end type write_statement_node

    ! Read statement node
    type, extends(ast_node), public :: read_statement_node
        character(len=:), allocatable :: unit_spec     ! Unit specifier (e.g., "10", "*")
        character(len=:), allocatable :: format_spec   ! Optional format
        integer, allocatable :: var_indices(:)         ! Variables to read into
        integer :: iostat_var_index = 0                 ! Optional iostat variable index
        integer :: err_label_index = 0                  ! Optional err label index
        integer :: end_label_index = 0                  ! Optional end label index
        integer :: format_expr_index = 0                ! Optional runtime format expression
        logical :: is_formatted = .false.               ! True if formatted I/O
    contains
        procedure :: accept => read_statement_accept
        procedure :: to_json => read_statement_to_json
    end type read_statement_node

    ! Format descriptor node for parsed format specifications
    type, extends(ast_node), public :: format_descriptor_node
        character(len=:), allocatable :: descriptor_type  ! I, F, E, A, X, etc.
        integer :: width = 0                             ! Field width
        integer :: decimal_places = 0                     ! Decimal places (for F, E)
        integer :: exponent_width = 0                     ! Exponent width (for E)
        integer :: repeat_count = 1                       ! Repetition count
        logical :: is_literal = .false.                  ! True for literal strings
        character(len=:), allocatable :: literal_text     ! For literal format strings
    contains
        procedure :: accept => format_descriptor_accept
        procedure :: to_json => format_descriptor_to_json
    end type format_descriptor_node

    ! Allocate statement node
    type, extends(ast_node), public :: allocate_statement_node
        integer, allocatable :: var_indices(:)         ! Variables to allocate
        integer, allocatable :: shape_indices(:)       ! Shape expressions for each variable
        integer :: stat_var_index = 0                  ! Optional stat variable index
        integer :: errmsg_var_index = 0                ! Optional errmsg variable index
        integer :: source_expr_index = 0               ! Optional source expression index
        integer :: mold_expr_index = 0                 ! Optional mold expression index
    contains
        procedure :: accept => allocate_statement_accept
        procedure :: to_json => allocate_statement_to_json
    end type allocate_statement_node

    ! Deallocate statement node
    type, extends(ast_node), public :: deallocate_statement_node
        integer, allocatable :: var_indices(:)         ! Variables to deallocate
        integer :: stat_var_index = 0                  ! Optional stat variable index
        integer :: errmsg_var_index = 0                ! Optional errmsg variable index
    contains
        procedure :: accept => deallocate_statement_accept
        procedure :: to_json => deallocate_statement_to_json
    end type deallocate_statement_node

    ! Declaration node
    type, extends(ast_node), public :: declaration_node
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        character(len=:), allocatable :: var_name      ! Variable name
        integer :: kind_value                          ! Kind parameter (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        character(len=:), allocatable :: intent        ! in, out, inout (for parameters)
        logical :: has_intent = .false.                ! Whether intent was specified
        class(ast_node), allocatable :: initializer    ! Optional initialization value (legacy)
        integer :: initializer_index = 0              ! Initializer index (stack-based)
        logical :: has_initializer = .false.          ! Whether initializer is present
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is an array declaration
        type(ast_node_wrapper), allocatable :: dimensions(:) ! Array dimensions (legacy)
        integer, allocatable :: dimension_indices(:)  ! Dimension indices (stack-based)
        logical :: is_allocatable = .false.           ! Whether allocatable attribute is present
        logical :: is_pointer = .false.                ! Whether pointer attribute is present
    contains
        procedure :: accept => declaration_accept
        procedure :: to_json => declaration_to_json
    end type declaration_node

    ! Parameter declaration node (for function/subroutine parameters)
    type, extends(ast_node), public :: parameter_declaration_node
        character(len=:), allocatable :: name          ! Parameter name
        character(len=:), allocatable :: type_name     ! real, integer, etc.
        integer :: kind_value                          ! Kind parameter (e.g., 8 for real(8))
        logical :: has_kind                            ! Whether kind was specified
        character(len=:), allocatable :: intent        ! in, out, inout
        logical :: has_intent                          ! Whether intent was specified
        ! Array dimension support
        logical :: is_array = .false.                  ! Whether this is an array parameter
        integer, allocatable :: dimension_indices(:)   ! Dimension indices (stack-based)
    contains
        procedure :: accept => parameter_declaration_accept
        procedure :: to_json => parameter_declaration_to_json
    end type parameter_declaration_node

    ! Contains statement node (for separating interface from implementation)
    type, extends(ast_node), public :: contains_node
    contains
        procedure :: accept => contains_accept
        procedure :: to_json => contains_to_json
    end type contains_node

    ! Do loop node
    type, extends(ast_node), public :: do_loop_node
        character(len=:), allocatable :: var_name     ! Loop variable
        character(len=:), allocatable :: label        ! Loop label (optional)
        integer :: start_expr_index = 0               ! Start expression arena index
        integer :: end_expr_index = 0                 ! End expression arena index
        integer :: step_expr_index = 0                ! Step expression arena index (optional)
        integer, allocatable :: body_indices(:)       ! Loop body arena indices
    contains
        procedure :: accept => do_loop_accept
        procedure :: to_json => do_loop_to_json
        procedure :: assign => do_loop_assign
        generic :: assignment(=) => assign
    end type do_loop_node

    ! Do while loop node
    type, extends(ast_node), public :: do_while_node
        integer :: condition_index = 0                ! While condition arena index
        integer, allocatable :: body_indices(:)       ! Loop body arena indices
    contains
        procedure :: accept => do_while_accept
        procedure :: to_json => do_while_to_json
    end type do_while_node

    ! Forall construct node
    type, extends(ast_node), public :: forall_node
        character(len=:), allocatable :: index_var        ! Index variable name
        integer :: start_index = 0                        ! Start expression arena index
        integer :: end_index = 0                          ! End expression arena index
        integer :: step_index = 0                         ! Step expression arena index (optional)
        integer :: mask_index = 0                         ! Mask condition arena index (optional)
        integer, allocatable :: body_indices(:)           ! Forall body arena indices
    contains
        procedure :: accept => forall_accept
        procedure :: to_json => forall_to_json
        procedure :: assign => forall_assign
        generic :: assignment(=) => assign
    end type forall_node

    ! If statement node
    type, extends(ast_node), public :: if_node
        integer :: condition_index = 0                ! If condition arena index
        integer, allocatable :: then_body_indices(:) ! Then body arena indices
        type(elseif_wrapper), allocatable :: elseif_blocks(:) ! Elseif blocks (optional)
        integer, allocatable :: else_body_indices(:) ! Else body arena indices (optional)
    contains
        procedure :: accept => if_accept
        procedure :: to_json => if_to_json
    end type if_node

    ! Elseif wrapper (not an AST node itself)
    type, public :: elseif_wrapper
        integer :: condition_index = 0                ! Elseif condition arena index
        integer, allocatable :: body_indices(:)       ! Elseif body arena indices
    end type elseif_wrapper

    ! Select case construct node
    type, extends(ast_node), public :: select_case_node
        integer :: selector_index = 0                 ! Selector expression arena index
        integer, allocatable :: case_indices(:)       ! Case block arena indices
        integer :: default_index = 0                  ! Default case arena index (optional)
    contains
        procedure :: accept => select_case_accept
        procedure :: to_json => select_case_to_json
        procedure :: assign => select_case_assign
        generic :: assignment(=) => assign
    end type select_case_node

    ! Case block node (case (values) body)
    type, extends(ast_node), public :: case_block_node
        integer, allocatable :: value_indices(:)      ! Case value arena indices
        integer, allocatable :: body_indices(:)       ! Case body arena indices
    contains
        procedure :: accept => case_block_accept
        procedure :: to_json => case_block_to_json
        procedure :: assign => case_block_assign
        generic :: assignment(=) => assign
    end type case_block_node

    ! Case range node (for ranges like 1:5)
    type, extends(ast_node), public :: case_range_node
        integer :: start_value = 0                    ! Start value
        integer :: end_value = 0                      ! End value
    contains
        procedure :: accept => case_range_accept
        procedure :: to_json => case_range_to_json
        procedure :: assign => case_range_assign
        generic :: assignment(=) => assign
    end type case_range_node

    ! Case default node (case default body)
    type, extends(ast_node), public :: case_default_node
        integer, allocatable :: body_indices(:)       ! Default case body arena indices
    contains
        procedure :: accept => case_default_accept
        procedure :: to_json => case_default_to_json
        procedure :: assign => case_default_assign
        generic :: assignment(=) => assign
    end type case_default_node

    ! Derived type definition node
    type, extends(ast_node), public :: derived_type_node
        character(len=:), allocatable :: name          ! Type name
        type(ast_node_wrapper), allocatable :: components(:) ! Type components (legacy)
        integer, allocatable :: component_indices(:)   ! Component indices (stack-based)
        logical :: has_parameters = .false.            ! Whether it has parameters
        type(ast_node_wrapper), allocatable :: parameters(:) ! Type parameters (legacy)
        integer, allocatable :: param_indices(:)       ! Parameter indices (stack-based)
    contains
        procedure :: accept => derived_type_accept
        procedure :: to_json => derived_type_to_json
    end type derived_type_node

    ! Interface block node
    type, extends(ast_node), public :: interface_block_node
        character(len=:), allocatable :: name         ! Interface name (optional)
        character(len=:), allocatable :: kind         ! "interface", "generic", "operator", "assignment"
        character(len=:), allocatable :: operator     ! Operator symbol (for operator interfaces)
        integer, allocatable :: procedure_indices(:)  ! Procedure declaration arena indices
    contains
        procedure :: accept => interface_block_accept
        procedure :: to_json => interface_block_to_json
    end type interface_block_node

    ! Module node
    type, extends(ast_node), public :: module_node
        character(len=:), allocatable :: name         ! Module name
        integer, allocatable :: declaration_indices(:) ! Module declaration arena indices
        integer, allocatable :: procedure_indices(:)   ! Module procedure arena indices (after contains)
        logical :: has_contains = .false.             ! Whether module has a contains section
    contains
        procedure :: accept => module_accept
        procedure :: to_json => module_to_json
    end type module_node

    ! STOP statement node
    type, extends(ast_node), public :: stop_node
        integer :: stop_code_index = 0                ! Optional stop code expression index
        character(len=:), allocatable :: stop_message ! Optional stop message string
    contains
        procedure :: accept => stop_accept
        procedure :: to_json => stop_to_json
    end type stop_node

    ! RETURN statement node
    type, extends(ast_node), public :: return_node
        ! RETURN statement has no additional data
    contains
        procedure :: accept => return_accept
        procedure :: to_json => return_to_json
    end type return_node

    ! CYCLE statement node
    type, extends(ast_node), public :: cycle_node
        character(len=:), allocatable :: loop_label  ! Optional loop label to cycle
    contains
        procedure :: accept => cycle_accept
        procedure :: to_json => cycle_to_json
    end type cycle_node

    ! EXIT statement node
    type, extends(ast_node), public :: exit_node
        character(len=:), allocatable :: loop_label  ! Optional loop label to exit
    contains
        procedure :: accept => exit_accept
        procedure :: to_json => exit_to_json
    end type exit_node

    ! WHERE construct node
    type, extends(ast_node), public :: where_node
        integer :: mask_expr_index = 0                ! Mask expression index
        integer, allocatable :: where_body_indices(:) ! WHERE body statement indices
        integer, allocatable :: elsewhere_body_indices(:) ! ELSEWHERE body statement indices (optional)
    contains
        procedure :: accept => where_accept
        procedure :: to_json => where_to_json
    end type where_node

    ! Case statement wrapper (temporary for parser compatibility)
    type, public :: case_wrapper
        character(len=:), allocatable :: case_type    ! "case", "case_default"
        class(ast_node), allocatable :: value         ! Case value (optional for default)
        type(ast_node_wrapper), allocatable :: body(:) ! Case body
    end type case_wrapper

    ! Wrapper type for polymorphic arrays - BUT NOW BACKED BY STACK
    type, public :: ast_node_wrapper
        class(ast_node), allocatable :: node
        integer :: stack_index = 0  ! NEW: Index in AST stack for O(depth) access
    end type ast_node_wrapper

    ! Literal kind constants
    integer, parameter, public :: LITERAL_INTEGER = 1
    integer, parameter, public :: LITERAL_REAL = 2
    integer, parameter, public :: LITERAL_STRING = 3
    integer, parameter, public :: LITERAL_LOGICAL = 4
    integer, parameter, public :: LITERAL_ARRAY = 5
    integer, parameter, public :: LITERAL_COMPLEX = 6

    ! Public interface for creating nodes and stack
    public :: create_ast_stack
public :: create_program, create_assignment, create_pointer_assignment, create_binary_op
    public :: create_function_def, create_subroutine_def, create_call_or_subscript, create_subroutine_call
    public :: create_identifier, create_literal, create_array_literal
    public :: create_use_statement, create_include_statement, create_print_statement, &
              create_write_statement, create_read_statement
    public :: create_declaration, create_do_loop, create_do_while, create_if, create_select_case
    public :: create_derived_type, create_interface_block, create_module
    public :: create_stop, create_return
    public :: create_cycle, create_exit
    public :: create_where

contains

    ! Create a new high-performance AST arena
    function create_ast_stack(initial_capacity) result(arena)
        integer, intent(in), optional :: initial_capacity
        type(ast_arena_t) :: arena
        integer :: cap

        ! Use chunk-aligned initial capacity for optimal performance
        if (present(initial_capacity)) then
            cap = max(initial_capacity, arena%initial_capacity)
        else
            cap = arena%initial_capacity
        end if

        arena%capacity = cap
        arena%chunk_size = 1024  ! High-performance chunk size
        arena%initial_capacity = 256
        allocate (arena%entries(cap))
        arena%size = 0
        arena%current_index = 0
        arena%max_depth = 0
    end function create_ast_stack

    ! Push a new AST node onto the stack
    subroutine ast_arena_push(this, node, node_type, parent_index)
        class(ast_arena_t), intent(inout) :: this
        class(ast_node), intent(in) :: node
        character(len=*), intent(in), optional :: node_type
        integer, intent(in), optional :: parent_index
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: new_capacity, parent_depth, parent_idx

        ! Grow array using buffered chunk allocation for high performance
        if (this%size >= this%capacity) then
            if (this%capacity == 0) then
                new_capacity = this%initial_capacity
            else
                ! Grow by chunk_size to minimize allocations
                new_capacity = this%capacity + this%chunk_size
            end if
            allocate (temp_entries(new_capacity))
            if (this%size > 0) then
                temp_entries(1:this%size) = this%entries(1:this%size)
            end if
            this%entries = temp_entries
            this%capacity = new_capacity
        end if

        ! Add new entry
        this%size = this%size + 1
        this%current_index = this%size

        ! Store the node using allocatable source
        allocate (this%entries(this%size)%node, source=node)

        ! Set parent index and depth
        if (present(parent_index)) then
            parent_idx = parent_index
        else
            parent_idx = 0  ! Root node
        end if

        this%entries(this%size)%parent_index = parent_idx

        if (parent_idx == 0) then
            this%entries(this%size)%depth = 0
        else
            ! Validate parent index is within bounds
            if (parent_idx < 1 .or. parent_idx > this%size - 1) then
                ! Invalid parent index - treat as root node
                this%entries(this%size)%depth = 0
            else
                parent_depth = this%entries(parent_idx)%depth
                this%entries(this%size)%depth = parent_depth + 1
            end if
        end if

        ! Update max depth
        if (this%entries(this%size)%depth > this%max_depth) then
            this%max_depth = this%entries(this%size)%depth
        end if

        ! Store type name for debugging
        if (present(node_type)) then
            this%entries(this%size)%node_type = node_type
        else
            this%entries(this%size)%node_type = "unknown"
        end if

        ! Initialize child array
        this%entries(this%size)%child_count = 0

        ! Add to parent's child list
        if (parent_idx > 0) then
            call this%add_child(parent_idx, this%size)
        end if
    end subroutine ast_arena_push

    ! Add a child to a parent node
    subroutine ast_arena_add_child(this, parent_index, child_index)
        class(ast_arena_t), intent(inout) :: this
        integer, intent(in) :: parent_index, child_index
        integer, allocatable :: temp_children(:)
        integer :: new_size

        if (parent_index <= 0 .or. parent_index > this%size) return
        if (child_index <= 0 .or. child_index > this%size) return

        ! Grow child array if needed
        if (.not. allocated(this%entries(parent_index)%child_indices)) then
            allocate (this%entries(parent_index)%child_indices(10))
        else if (this%entries(parent_index)%child_count >= size(this%entries(parent_index)%child_indices)) then
            new_size = size(this%entries(parent_index)%child_indices)*2
            allocate (temp_children(new_size))
            temp_children(1:this%entries(parent_index)%child_count) = &
      this%entries(parent_index)%child_indices(1:this%entries(parent_index)%child_count)
            this%entries(parent_index)%child_indices = temp_children
        end if

        ! Add child
     this%entries(parent_index)%child_count = this%entries(parent_index)%child_count + 1
        this%entries(parent_index)%child_indices(this%entries(parent_index)%child_count) = child_index
    end subroutine ast_arena_add_child

    ! Pop the current node from arena (with memory cleanup)
    subroutine ast_arena_pop(this)
        class(ast_arena_t), intent(inout) :: this

        if (this%size > 0) then
            ! Clean up the node allocation (ordered release)
            if (allocated(this%entries(this%size)%node)) then
                deallocate (this%entries(this%size)%node)
            end if
            if (allocated(this%entries(this%size)%child_indices)) then
                deallocate (this%entries(this%size)%child_indices)
            end if

            this%size = this%size - 1
            if (this%size > 0) then
                this%current_index = this%size
            else
                this%current_index = 0
            end if

            ! Shrink arena if using less than 25% of capacity and over chunk size
            if (this%capacity > this%chunk_size .and. &
                this%size < this%capacity/4) then
                call this%shrink_arena()
            end if
        else
            error stop "Cannot pop from empty AST arena"
        end if
    end subroutine ast_arena_pop

    ! Get the current node from the stack
    function ast_arena_current(this) result(node)
        class(ast_arena_t), intent(in) :: this
        class(ast_node), allocatable :: node

        if (this%current_index > 0 .and. this%current_index <= this%size) then
            if (allocated(this%entries(this%current_index)%node)) then
                allocate (node, source=this%entries(this%current_index)%node)
            end if
        end if
    end function ast_arena_current

    ! Get the parent of the current node
    function ast_arena_get_parent(this, index) result(parent_node)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in), optional :: index
        class(ast_node), allocatable :: parent_node
        integer :: idx, parent_idx

        idx = this%current_index
        if (present(index)) idx = index

        if (idx > 0 .and. idx <= this%size) then
            parent_idx = this%entries(idx)%parent_index
            if (parent_idx > 0) then
                allocate (parent_node, source=this%entries(parent_idx)%node)
            end if
        end if
    end function ast_arena_get_parent

    ! Get the depth of a node (or current node)
    function ast_arena_get_depth(this, index) result(depth)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in), optional :: index
        integer :: depth, idx

        idx = this%current_index
        if (present(index)) idx = index

        depth = -1  ! Invalid depth
        if (idx > 0 .and. idx <= this%size) then
            depth = this%entries(idx)%depth
        end if
    end function ast_arena_get_depth

    ! Traverse nodes at a specific depth (O(depth) complexity)
    subroutine ast_arena_traverse_depth(this, target_depth, visitor)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: target_depth
        class(*), intent(inout) :: visitor
        integer :: i

        ! Linear scan through stack entries (cache-efficient)
        do i = 1, this%size
            if (this%entries(i)%depth == target_depth) then
                if (allocated(this%entries(i)%node)) then
                    call this%entries(i)%node%accept(visitor)
                end if
            end if
        end do
    end subroutine ast_arena_traverse_depth

    ! Find nodes by type (O(n) but cache-efficient)
    function ast_arena_find_by_type(this, node_type) result(indices)
        class(ast_arena_t), intent(in) :: this
        character(len=*), intent(in) :: node_type
        integer, allocatable :: indices(:)
        integer, allocatable :: temp_indices(:)
        integer :: i, count

        ! Count matching nodes
        count = 0
        do i = 1, this%size
            if (allocated(this%entries(i)%node_type)) then
                if (this%entries(i)%node_type == node_type) then
                    count = count + 1
                end if
            end if
        end do

        ! Allocate result array
        if (count > 0) then
            allocate (temp_indices(count))
            count = 0
            do i = 1, this%size
                if (allocated(this%entries(i)%node_type)) then
                    if (this%entries(i)%node_type == node_type) then
                        count = count + 1
                        temp_indices(count) = i
                    end if
                end if
            end do
            indices = temp_indices
        else
            allocate (indices(0))
        end if
    end function ast_arena_find_by_type

    ! Get children of a node (O(1) lookup)
    function ast_arena_get_children(this, parent_index) result(child_indices)
        class(ast_arena_t), intent(in) :: this
        integer, intent(in) :: parent_index
        integer, allocatable :: child_indices(:)

        if (parent_index > 0 .and. parent_index <= this%size) then
            if (allocated(this%entries(parent_index)%child_indices)) then
                allocate (child_indices(this%entries(parent_index)%child_count))
                child_indices = this%entries(parent_index)%child_indices(1:this%entries(parent_index)%child_count)
            else
                allocate (child_indices(0))
            end if
        else
            allocate (child_indices(0))
        end if
    end function ast_arena_get_children

    ! Get performance statistics
    function ast_arena_get_stats(this) result(stats)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_stats_t) :: stats

        stats%total_nodes = this%size
        stats%max_depth = this%max_depth
        stats%capacity = this%capacity
        stats%memory_usage = this%capacity*100  ! Rough estimate in bytes
    end function ast_arena_get_stats

    ! Clear the stack
    subroutine ast_arena_clear(this)
        class(ast_arena_t), intent(inout) :: this
        integer :: i

        ! Clean up all node allocations in reverse order (ordered release)
        do i = this%size, 1, -1
            if (allocated(this%entries(i)%node)) then
                deallocate (this%entries(i)%node)
            end if
            if (allocated(this%entries(i)%child_indices)) then
                deallocate (this%entries(i)%child_indices)
            end if
        end do

        this%size = 0
        this%current_index = 0
        this%max_depth = 0

        ! Reset to initial capacity to prevent memory leaks
        if (allocated(this%entries)) then
            deallocate (this%entries)
            allocate (this%entries(this%initial_capacity))
            this%capacity = this%initial_capacity
        end if
    end subroutine ast_arena_clear

    ! Factory functions for creating AST nodes (KEEP ALL ORIGINAL SIGNATURES)

    function create_program(name, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(program_node) :: node
        integer :: i

        node%name = name
        if (size(body_indices) > 0) then
            allocate (node%body_indices(size(body_indices)))
            do i = 1, size(body_indices)
                node%body_indices(i) = body_indices(i)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_program

    function create_assignment(target_index, value_index, line, column, inferred_type, inferred_type_name) result(node)
        integer, intent(in) :: target_index
        integer, intent(in) :: value_index
        integer, intent(in), optional :: line, column
        logical, intent(in), optional :: inferred_type
        character(len=*), intent(in), optional :: inferred_type_name
        type(assignment_node) :: node

        node%target_index = target_index
        node%value_index = value_index
        if (present(line)) node%line = line
        if (present(column)) node%column = column
        if (present(inferred_type)) node%type_was_inferred = inferred_type
        if (present(inferred_type_name)) node%inferred_type_name = inferred_type_name
    end function create_assignment

    function create_pointer_assignment(pointer_index, target_index, line, column) result(node)
        integer, intent(in) :: pointer_index
        integer, intent(in) :: target_index
        integer, intent(in), optional :: line, column
        type(pointer_assignment_node) :: node

        node%pointer_index = pointer_index
        node%target_index = target_index
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_pointer_assignment

 function create_binary_op(left_index, right_index, operator, line, column) result(node)
        integer, intent(in) :: left_index
        integer, intent(in) :: right_index
        character(len=*), intent(in) :: operator
        integer, intent(in), optional :: line, column
        type(binary_op_node) :: node

        node%left_index = left_index
        node%right_index = right_index
        node%operator = operator
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_binary_op

function create_function_def(name, param_indices, return_type, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: param_indices(:)
        character(len=*), intent(in) :: return_type
        integer, intent(in) :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(function_def_node) :: node

        node%name = name
        if (size(param_indices) > 0) then
            node%param_indices = param_indices
        end if
        node%return_type = return_type
        if (size(body_indices) > 0) then
            node%body_indices = body_indices
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_function_def

    function create_subroutine_def(name, param_indices, body_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: param_indices(:)
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(subroutine_def_node) :: node

        node%name = name
        if (present(param_indices)) then
            if (size(param_indices) > 0) then
                node%param_indices = param_indices
            end if
        end if
        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_def

    function create_subroutine_call(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(subroutine_call_node) :: node
        integer :: i

        node%name = name
        if (size(args) > 0) then
            allocate (node%arg_indices(size(args)))
            do i = 1, size(args)
                node%arg_indices(i) = args(i)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_subroutine_call

    function create_call_or_subscript(name, args, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in) :: args(:)
        integer, intent(in), optional :: line, column
        type(call_or_subscript_node) :: node
        integer :: i

        node%name = name
        if (size(args) > 0) then
            allocate (node%arg_indices(size(args)))
            do i = 1, size(args)
                node%arg_indices(i) = args(i)
            end do
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_call_or_subscript

    function create_identifier(name, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: line, column
        type(identifier_node) :: node

        node%name = name
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_identifier

    function create_literal(value, kind, line, column) result(node)
        character(len=*), intent(in) :: value
        integer, intent(in) :: kind
        integer, intent(in), optional :: line, column
        type(literal_node) :: node

        node%value = value
        node%literal_kind = kind
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_literal

    function create_array_literal(element_indices, line, column) result(node)
        integer, intent(in) :: element_indices(:)
        integer, intent(in), optional :: line, column
        type(array_literal_node) :: node
        node%element_indices = element_indices
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_array_literal

    function create_declaration(type_name, var_name, kind_value, initializer, dimensions, &
                               is_allocatable, is_pointer, line, column) result(node)
        character(len=*), intent(in) :: type_name
        character(len=*), intent(in) :: var_name
        integer, intent(in), optional :: kind_value
        class(ast_node), allocatable, intent(in), optional :: initializer
        type(ast_node_wrapper), intent(in), optional :: dimensions(:)
        logical, intent(in), optional :: is_allocatable
        logical, intent(in), optional :: is_pointer
        integer, intent(in), optional :: line, column
        type(declaration_node) :: node

        node%type_name = type_name
        node%var_name = var_name

        if (present(kind_value)) then
            node%kind_value = kind_value
            node%has_kind = .true.
        else
            node%kind_value = 0
            node%has_kind = .false.
        end if

        if (present(initializer)) then
            allocate (node%initializer, source=initializer)
        end if

        if (present(dimensions)) then
            node%is_array = .true.
            allocate (node%dimensions, source=dimensions)
        else
            node%is_array = .false.
        end if

        if (present(is_allocatable)) then
            node%is_allocatable = is_allocatable
        else
            node%is_allocatable = .false.
        end if

        if (present(is_pointer)) then
            node%is_pointer = is_pointer
        else
            node%is_pointer = .false.
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_declaration

    function create_parameter_declaration(name, type_name, kind_value, intent, dimension_indices, line, column) result(node)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_name
        integer, intent(in), optional :: kind_value
        character(len=*), intent(in), optional :: intent
        integer, intent(in), optional :: dimension_indices(:)
        integer, intent(in), optional :: line, column
        type(parameter_declaration_node) :: node

        node%name = name
        node%type_name = type_name

        if (present(kind_value)) then
            node%kind_value = kind_value
            node%has_kind = .true.
        else
            node%kind_value = 0
            node%has_kind = .false.
        end if

        if (present(intent)) then
            node%intent = intent
            node%has_intent = .true.
        else
            node%intent = "in"  ! Default to intent(in)
            node%has_intent = .false.
        end if

        if (present(dimension_indices) .and. size(dimension_indices) > 0) then
            node%is_array = .true.
            node%dimension_indices = dimension_indices
        else
            node%is_array = .false.
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_parameter_declaration

    function create_use_statement(module_name, only_list, rename_list, has_only, line, column) result(node)
        character(len=*), intent(in) :: module_name
        character(len=*), intent(in), optional :: only_list(:)
        character(len=*), intent(in), optional :: rename_list(:)
        logical, intent(in), optional :: has_only
        integer, intent(in), optional :: line, column
        type(use_statement_node) :: node

        node%module_name = module_name
        if (present(only_list)) then
            allocate (node%only_list(size(only_list)))
            block
                integer :: i
                do i = 1, size(only_list)
                    node%only_list(i)%s = trim(only_list(i))
                end do
            end block
        end if
        if (present(rename_list)) then
            allocate (node%rename_list(size(rename_list)))
            block
                integer :: i
                do i = 1, size(rename_list)
                    node%rename_list(i)%s = trim(rename_list(i))
                end do
            end block
        end if
        if (present(has_only)) then
            node%has_only = has_only
        end if
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_use_statement

    function create_include_statement(filename, line, column) result(node)
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: line, column
        type(include_statement_node) :: node

        node%filename = filename
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_include_statement

    function create_print_statement(arg_indices, format_spec, line, column) result(node)
        integer, intent(in) :: arg_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(print_statement_node) :: node

        if (size(arg_indices) > 0) then
            node%arg_indices = arg_indices
        end if
        if (present(format_spec)) node%format_spec = format_spec
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_print_statement

    function create_write_statement(unit_spec, arg_indices, format_spec, line, column) result(node)
        character(len=*), intent(in) :: unit_spec
        integer, intent(in) :: arg_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(write_statement_node) :: node

        node%unit_spec = unit_spec
        if (size(arg_indices) > 0) then
            node%arg_indices = arg_indices
        end if
        if (present(format_spec)) node%format_spec = format_spec
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_write_statement

    function create_read_statement(unit_spec, var_indices, format_spec, line, column) result(node)
        character(len=*), intent(in) :: unit_spec
        integer, intent(in) :: var_indices(:)
        character(len=*), intent(in), optional :: format_spec
        integer, intent(in), optional :: line, column
        type(read_statement_node) :: node

        node%unit_spec = unit_spec
        if (size(var_indices) > 0) then
            node%var_indices = var_indices
        end if
        if (present(format_spec)) node%format_spec = format_spec
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_read_statement

    function create_do_loop(var_name, start_expr_index, end_expr_index, step_expr_index, body_indices, line, column) result(node)
        character(len=*), intent(in) :: var_name
        integer, intent(in) :: start_expr_index, end_expr_index
        integer, intent(in), optional :: step_expr_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_loop_node) :: node

        node%var_name = var_name
        node%start_expr_index = start_expr_index
        node%end_expr_index = end_expr_index
        if (present(step_expr_index)) node%step_expr_index = step_expr_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_loop

    function create_do_while(condition_index, body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: body_indices(:)
        integer, intent(in), optional :: line, column
        type(do_while_node) :: node

        node%condition_index = condition_index

        if (present(body_indices)) then
            if (size(body_indices) > 0) then
                node%body_indices = body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_do_while

    function create_if(condition_index, then_body_indices, elseif_blocks, else_body_indices, line, column) result(node)
        integer, intent(in) :: condition_index
        integer, intent(in), optional :: then_body_indices(:)
        type(elseif_wrapper), intent(in), optional :: elseif_blocks(:)
        integer, intent(in), optional :: else_body_indices(:)
        integer, intent(in), optional :: line, column
        type(if_node) :: node

        node%condition_index = condition_index

        if (present(then_body_indices) .and. size(then_body_indices) > 0) then
            node%then_body_indices = then_body_indices
        end if

        if (present(elseif_blocks) .and. size(elseif_blocks) > 0) then
            allocate (node%elseif_blocks, source=elseif_blocks)
        end if

        if (present(else_body_indices) .and. size(else_body_indices) > 0) then
            node%else_body_indices = else_body_indices
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_if

    function create_select_case(expr, cases, line, column) result(node)
        class(ast_node), intent(in) :: expr
        type(case_wrapper), intent(in), optional :: cases(:)
        integer, intent(in), optional :: line, column
        type(select_case_node) :: node

        ! This is a placeholder implementation for parser compatibility
        ! The actual select case logic uses the new AST design in factory functions
        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_select_case

   function create_derived_type(name, components, parameters, line, column) result(node)
        character(len=*), intent(in) :: name
        type(ast_node_wrapper), intent(in), optional :: components(:)
        type(ast_node_wrapper), intent(in), optional :: parameters(:)
        integer, intent(in), optional :: line, column
        type(derived_type_node) :: node

        node%name = name

        if (present(components)) then
            if (size(components) > 0) then
                allocate (node%components, source=components)
            end if
        end if

        if (present(parameters)) then
            if (size(parameters) > 0) then
                node%has_parameters = .true.
                allocate (node%parameters, source=parameters)
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_derived_type

    function create_interface_block(name, kind, operator, procedure_indices, line, column) result(node)
        character(len=*), intent(in), optional :: name
        character(len=*), intent(in) :: kind
        character(len=*), intent(in), optional :: operator
        integer, intent(in), optional :: procedure_indices(:)
        integer, intent(in), optional :: line, column
        type(interface_block_node) :: node

        if (present(name)) then
            node%name = name
        end if

        node%kind = kind

        if (present(operator)) then
            node%operator = operator
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_interface_block

    function create_module(name, declaration_indices, procedure_indices, has_contains, line, column) result(node)
        character(len=*), intent(in) :: name
        integer, intent(in), optional :: declaration_indices(:)
        integer, intent(in), optional :: procedure_indices(:)
        logical, intent(in), optional :: has_contains
        integer, intent(in), optional :: line, column
        type(module_node) :: node

        node%name = name

        if (present(declaration_indices)) then
            if (size(declaration_indices) > 0) then
                node%declaration_indices = declaration_indices
            end if
        end if

        if (present(procedure_indices)) then
            if (size(procedure_indices) > 0) then
                node%procedure_indices = procedure_indices
            end if
        end if

        if (present(has_contains)) then
            node%has_contains = has_contains
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_module

    ! Create a STOP node
    function create_stop(stop_code_index, stop_message, line, column) result(node)
        integer, intent(in), optional :: stop_code_index
        character(len=*), intent(in), optional :: stop_message
        integer, intent(in), optional :: line, column
        type(stop_node) :: node

        if (present(stop_code_index)) then
            node%stop_code_index = stop_code_index
        end if

        if (present(stop_message)) then
            node%stop_message = stop_message
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_stop

    ! Create a RETURN node
    function create_return(line, column) result(node)
        integer, intent(in), optional :: line, column
        type(return_node) :: node

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_return

    ! Create a CYCLE node
    function create_cycle(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(cycle_node) :: node

        if (present(loop_label)) then
            node%loop_label = loop_label
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_cycle

    ! Create an EXIT node
    function create_exit(loop_label, line, column) result(node)
        character(len=*), intent(in), optional :: loop_label
        integer, intent(in), optional :: line, column
        type(exit_node) :: node

        if (present(loop_label)) then
            node%loop_label = loop_label
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_exit

    ! Create a WHERE node
    function create_where(mask_expr_index, where_body_indices, elsewhere_body_indices, line, column) result(node)
        integer, intent(in) :: mask_expr_index
        integer, intent(in), optional :: where_body_indices(:)
        integer, intent(in), optional :: elsewhere_body_indices(:)
        integer, intent(in), optional :: line, column
        type(where_node) :: node

        node%mask_expr_index = mask_expr_index

        if (present(where_body_indices)) then
            if (size(where_body_indices) > 0) then
                node%where_body_indices = where_body_indices
            end if
        end if

        if (present(elsewhere_body_indices)) then
            if (size(elsewhere_body_indices) > 0) then
                node%elsewhere_body_indices = elsewhere_body_indices
            end if
        end if

        if (present(line)) node%line = line
        if (present(column)) node%column = column
    end function create_where

    ! JSON serialization implementations (I'll include the key ones)

    subroutine program_to_json(this, json, parent)
        class(program_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'program')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(body_array, 'body')
        call json%add(obj, body_array)

        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                block
                    type(json_value), pointer :: body_obj
                    call json%create_object(body_obj, '')
                    call json%add(body_obj, 'stack_index', this%body_indices(i))
                    call json%add(body_array, body_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine program_to_json

    subroutine assignment_to_json(this, json, parent)
        class(assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'assignment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'inferred_type', this%type_was_inferred)
        if (allocated(this%inferred_type_name)) then
            call json%add(obj, 'inferred_type_name', this%inferred_type_name)
        end if

        ! Add target and value as stack indices
        call json%add(obj, 'target_index', this%target_index)
        call json%add(obj, 'value_index', this%value_index)

        call json%add(parent, obj)
    end subroutine assignment_to_json

    subroutine pointer_assignment_to_json(this, json, parent)
        class(pointer_assignment_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'pointer_assignment')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add pointer and target as stack indices
        call json%add(obj, 'pointer_index', this%pointer_index)
        call json%add(obj, 'target_index', this%target_index)

        call json%add(parent, obj)
    end subroutine pointer_assignment_to_json

    subroutine binary_op_to_json(this, json, parent)
        class(binary_op_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'binary_op')
        call json%add(obj, 'operator', this%operator)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add left and right as stack indices
        call json%add(obj, 'left_index', this%left_index)
        call json%add(obj, 'right_index', this%right_index)

        call json%add(parent, obj)
    end subroutine binary_op_to_json

    subroutine function_def_to_json(this, json, parent)
        class(function_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'function_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(params_array, 'param_indices')
        call json%add(obj, params_array)
        if (allocated(this%param_indices)) then
            do i = 1, size(this%param_indices)
                block
                    type(json_value), pointer :: param_obj
                    call json%create_object(param_obj, '')
                    call json%add(param_obj, 'stack_index', this%param_indices(i))
                    call json%add(params_array, param_obj)
                end block
            end do
        end if

        call json%add(obj, 'return_type', this%return_type)

        call json%create_array(body_array, 'body_indices')
        call json%add(obj, body_array)
        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                block
                    type(json_value), pointer :: body_obj
                    call json%create_object(body_obj, '')
                    call json%add(body_obj, 'stack_index', this%body_indices(i))
                    call json%add(body_array, body_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine function_def_to_json

    subroutine subroutine_def_to_json(this, json, parent)
        class(subroutine_def_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, params_array, body_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_def')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(params_array, 'param_indices')
        call json%add(obj, params_array)
        if (allocated(this%param_indices)) then
            do i = 1, size(this%param_indices)
                block
                    type(json_value), pointer :: param_obj
                    call json%create_object(param_obj, '')
                    call json%add(param_obj, 'stack_index', this%param_indices(i))
                    call json%add(params_array, param_obj)
                end block
            end do
        end if

        call json%create_array(body_array, 'body_indices')
        call json%add(obj, body_array)
        if (allocated(this%body_indices)) then
            do i = 1, size(this%body_indices)
                block
                    type(json_value), pointer :: body_obj
                    call json%create_object(body_obj, '')
                    call json%add(body_obj, 'stack_index', this%body_indices(i))
                    call json%add(body_array, body_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine subroutine_def_to_json

    subroutine subroutine_call_to_json(this, json, parent)
        class(subroutine_call_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'subroutine_call')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine subroutine_call_to_json

    subroutine call_or_subscript_to_json(this, json, parent)
        class(call_or_subscript_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'call_or_subscript')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%create_array(args_array, 'args')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine call_or_subscript_to_json

    subroutine identifier_to_json(this, json, parent)
        class(identifier_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'identifier')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%add(parent, obj)
    end subroutine identifier_to_json

    subroutine literal_to_json(this, json, parent)
        class(literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        character(len=:), allocatable :: kind_name

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'literal')
        call json%add(obj, 'value', this%value)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        select case (this%literal_kind)
        case (LITERAL_INTEGER)
            kind_name = 'integer'
        case (LITERAL_REAL)
            kind_name = 'real'
        case (LITERAL_STRING)
            kind_name = 'string'
        case (LITERAL_LOGICAL)
            kind_name = 'logical'
        case (LITERAL_ARRAY)
            kind_name = 'array'
        case default
            kind_name = 'unknown'
        end select
        call json%add(obj, 'kind', kind_name)

        call json%add(parent, obj)
    end subroutine literal_to_json

    subroutine array_literal_accept(this, visitor)
        class(array_literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine array_literal_accept

    subroutine array_literal_to_json(this, json, parent)
        class(array_literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, elements_array
        integer :: i
        call json%create_object(obj, '')
        call json%add(obj, 'type', 'array_literal')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        if (allocated(this%element_indices)) then
            call json%create_array(elements_array, 'element_indices')
            call json%add(obj, elements_array)
            do i = 1, size(this%element_indices)
                call json%add(elements_array, '', this%element_indices(i))
            end do
        end if
        call json%add(parent, obj)
    end subroutine array_literal_to_json

    subroutine use_statement_to_json(this, json, parent)
        class(use_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, only_array, rename_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'use_statement')
        call json%add(obj, 'module_name', this%module_name)
        call json%add(obj, 'has_only', this%has_only)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%only_list)) then
            call json%create_array(only_array, 'only_list')
            call json%add(obj, only_array)
            do i = 1, size(this%only_list)
                if (allocated(this%only_list(i)%s)) then
                    call json%add(only_array, '', this%only_list(i)%s)
                else
                    call json%add(only_array, '', '')
                end if
            end do
        end if

        if (allocated(this%rename_list)) then
            call json%create_array(rename_array, 'rename_list')
            call json%add(obj, rename_array)
            do i = 1, size(this%rename_list)
                if (allocated(this%rename_list(i)%s)) then
                    call json%add(rename_array, '', this%rename_list(i)%s)
                else
                    call json%add(rename_array, '', '')
                end if
            end do
        end if

        call json%add(parent, obj)
    end subroutine use_statement_to_json

    subroutine include_statement_to_json(this, json, parent)
        class(include_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'include_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'filename', this%filename)

        call json%add(parent, obj)
    end subroutine include_statement_to_json

    subroutine print_statement_to_json(this, json, parent)
        class(print_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'print_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if

        call json%create_array(args_array, 'arg_indices')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine print_statement_to_json

    subroutine write_statement_to_json(this, json, parent)
        class(write_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, args_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'write_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%unit_spec)) then
            call json%add(obj, 'unit_spec', this%unit_spec)
        end if

        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if

        call json%create_array(args_array, 'arg_indices')
        call json%add(obj, args_array)
        if (allocated(this%arg_indices)) then
            do i = 1, size(this%arg_indices)
                block
                    type(json_value), pointer :: arg_obj
                    call json%create_object(arg_obj, '')
                    call json%add(arg_obj, 'stack_index', this%arg_indices(i))
                    call json%add(args_array, arg_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine write_statement_to_json

    subroutine read_statement_to_json(this, json, parent)
        class(read_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, vars_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'read_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%unit_spec)) then
            call json%add(obj, 'unit_spec', this%unit_spec)
        end if

        if (allocated(this%format_spec)) then
            call json%add(obj, 'format_spec', this%format_spec)
        end if

        call json%create_array(vars_array, 'var_indices')
        call json%add(obj, vars_array)
        if (allocated(this%var_indices)) then
            do i = 1, size(this%var_indices)
                block
                    type(json_value), pointer :: var_obj
                    call json%create_object(var_obj, '')
                    call json%add(var_obj, 'stack_index', this%var_indices(i))
                    call json%add(vars_array, var_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine read_statement_to_json

    subroutine format_descriptor_to_json(this, json, parent)
        class(format_descriptor_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, "format_descriptor")
        
        call json%add(obj, "type", "format_descriptor")
        if (allocated(this%descriptor_type)) then
            call json%add(obj, "descriptor_type", this%descriptor_type)
        end if
        call json%add(obj, "width", this%width)
        call json%add(obj, "decimal_places", this%decimal_places)
        call json%add(obj, "exponent_width", this%exponent_width)
        call json%add(obj, "repeat_count", this%repeat_count)
        call json%add(obj, "is_literal", this%is_literal)
        if (allocated(this%literal_text)) then
            call json%add(obj, "literal_text", this%literal_text)
        end if
        call json%add(obj, "line", this%line)
        call json%add(obj, "column", this%column)

        call json%add(parent, obj)
    end subroutine format_descriptor_to_json

    subroutine allocate_statement_to_json(this, json, parent)
        class(allocate_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, vars_array, shapes_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'allocate_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%var_indices)) then
            call json%create_array(vars_array, 'var_indices')
            do i = 1, size(this%var_indices)
                call json%add(vars_array, '', this%var_indices(i))
            end do
            call json%add(obj, vars_array)
        end if

        if (allocated(this%shape_indices)) then
            call json%create_array(shapes_array, 'shape_indices')
            do i = 1, size(this%shape_indices)
                call json%add(shapes_array, '', this%shape_indices(i))
            end do
            call json%add(obj, shapes_array)
        end if

        if (this%stat_var_index > 0) then
            call json%add(obj, 'stat_var_index', this%stat_var_index)
        end if

        if (this%errmsg_var_index > 0) then
            call json%add(obj, 'errmsg_var_index', this%errmsg_var_index)
        end if

        if (this%source_expr_index > 0) then
            call json%add(obj, 'source_expr_index', this%source_expr_index)
        end if

        if (this%mold_expr_index > 0) then
            call json%add(obj, 'mold_expr_index', this%mold_expr_index)
        end if

        call json%add(parent, obj)
    end subroutine allocate_statement_to_json

    subroutine deallocate_statement_to_json(this, json, parent)
        class(deallocate_statement_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj, vars_array
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'deallocate_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%var_indices)) then
            call json%create_array(vars_array, 'var_indices')
            do i = 1, size(this%var_indices)
                call json%add(vars_array, '', this%var_indices(i))
            end do
            call json%add(obj, vars_array)
        end if

        if (this%stat_var_index > 0) then
            call json%add(obj, 'stat_var_index', this%stat_var_index)
        end if

        if (this%errmsg_var_index > 0) then
            call json%add(obj, 'errmsg_var_index', this%errmsg_var_index)
        end if

        call json%add(parent, obj)
    end subroutine deallocate_statement_to_json

    subroutine declaration_to_json(this, json, parent)
        class(declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'declaration')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'type_name', this%type_name)
        call json%add(obj, 'var_name', this%var_name)

        if (this%has_kind) then
            call json%add(obj, 'kind_value', this%kind_value)
        end if

        if (this%has_intent) then
            call json%add(obj, 'intent', this%intent)
        end if

        if (this%is_array) then
            call json%add(obj, 'is_array', .true.)
        end if

        if (this%is_allocatable) then
            call json%add(obj, 'is_allocatable', .true.)
        end if

        if (this%is_pointer) then
            call json%add(obj, 'is_pointer', .true.)
        end if

        if (this%is_array) then

            if (allocated(this%dimensions)) then
                block
                    type(json_value), pointer :: dims_array
                    call json%create_array(dims_array, 'dimensions')
                    do i = 1, size(this%dimensions)
                        if (allocated(this%dimensions(i)%node)) then
                            call this%dimensions(i)%node%to_json(json, dims_array)
                        end if
                    end do
                    call json%add(obj, dims_array)
                end block
            end if
        end if

        if (allocated(this%initializer)) then
            block
                type(json_value), pointer :: init_obj
                call json%create_object(init_obj, 'initializer')
                call this%initializer%to_json(json, init_obj)
                call json%add(obj, init_obj)
            end block
        end if

        call json%add(parent, obj)
    end subroutine declaration_to_json

    ! Parameter declaration node to JSON
    subroutine parameter_declaration_to_json(this, json, parent)
        class(parameter_declaration_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'parameter_declaration')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'type_name', this%type_name)

        if (this%has_kind) then
            call json%add(obj, 'kind_value', this%kind_value)
        end if

        if (this%has_intent) then
            call json%add(obj, 'intent', this%intent)
        end if

        if (this%is_array) then
            call json%add(obj, 'is_array', .true.)
            if (allocated(this%dimension_indices)) then
                call json%add(obj, 'dimension_indices', this%dimension_indices)
            end if
        end if

        call json%add(parent, obj)
    end subroutine parameter_declaration_to_json

    ! Contains node accept visitor
    subroutine contains_accept(this, visitor)
        class(contains_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! No children to visit
    end subroutine contains_accept

    ! Contains node to JSON
    subroutine contains_to_json(this, json, parent)
        class(contains_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'contains')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%add(parent, obj)
    end subroutine contains_to_json

    subroutine do_loop_to_json(this, json, parent)
        class(do_loop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'do_loop')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'var_name', this%var_name)
        if (allocated(this%label)) call json%add(obj, 'label', this%label)
        call json%add(parent, obj)
    end subroutine do_loop_to_json

    subroutine do_while_to_json(this, json, parent)
        class(do_while_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'do_while')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine do_while_to_json

    subroutine if_to_json(this, json, parent)
        class(if_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
      type(json_value), pointer :: obj, then_array, else_array, elseif_array, elseif_obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'if_statement')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add condition index
        call json%add(obj, 'condition_index', this%condition_index)

        ! Add then body indices
        if (allocated(this%then_body_indices)) then
            call json%create_array(then_array, 'then_body_indices')
            call json%add(obj, then_array)
            do i = 1, size(this%then_body_indices)
                block
                    type(json_value), pointer :: stmt_obj
                    call json%create_object(stmt_obj, '')
                    call json%add(stmt_obj, 'stack_index', this%then_body_indices(i))
                    call json%add(then_array, stmt_obj)
                end block
            end do
        end if

        ! Add elseif blocks (simplified arena-based)
        if (allocated(this%elseif_blocks)) then
            call json%create_array(elseif_array, 'elseif_blocks')
            call json%add(obj, elseif_array)
            do i = 1, size(this%elseif_blocks)
                call json%create_object(elseif_obj, '')
     call json%add(elseif_obj, 'condition_index', this%elseif_blocks(i)%condition_index)
                if (allocated(this%elseif_blocks(i)%body_indices)) then
                    block
                        type(json_value), pointer :: body_array
                        integer :: j
                        call json%create_array(body_array, 'body_indices')
                        call json%add(elseif_obj, body_array)
                        do j = 1, size(this%elseif_blocks(i)%body_indices)
                            block
                                type(json_value), pointer :: stmt_obj
                                call json%create_object(stmt_obj, '')
           call json%add(stmt_obj, 'stack_index', this%elseif_blocks(i)%body_indices(j))
                                call json%add(body_array, stmt_obj)
                            end block
                        end do
                    end block
                end if
                call json%add(elseif_array, elseif_obj)
            end do
        end if

        ! Add else body indices
        if (allocated(this%else_body_indices)) then
            call json%create_array(else_array, 'else_body_indices')
            call json%add(obj, else_array)
            do i = 1, size(this%else_body_indices)
                block
                    type(json_value), pointer :: stmt_obj
                    call json%create_object(stmt_obj, '')
                    call json%add(stmt_obj, 'stack_index', this%else_body_indices(i))
                    call json%add(else_array, stmt_obj)
                end block
            end do
        end if

        call json%add(parent, obj)
    end subroutine if_to_json

    subroutine derived_type_to_json(this, json, parent)
        class(derived_type_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'derived_type')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (this%has_parameters) then
            call json%add(obj, 'has_parameters', .true.)
            if (allocated(this%parameters)) then
                block
                    type(json_value), pointer :: params_array
                    call json%create_array(params_array, 'parameters')
                    do i = 1, size(this%parameters)
                        if (allocated(this%parameters(i)%node)) then
                            call this%parameters(i)%node%to_json(json, params_array)
                        end if
                    end do
                    call json%add(obj, params_array)
                end block
            end if
        end if

        if (allocated(this%components)) then
            block
                type(json_value), pointer :: components_array
                call json%create_array(components_array, 'components')
                do i = 1, size(this%components)
                    if (allocated(this%components(i)%node)) then
                        call this%components(i)%node%to_json(json, components_array)
                    end if
                end do
                call json%add(obj, components_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine derived_type_to_json

    subroutine interface_block_to_json(this, json, parent)
        class(interface_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'interface_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'kind', this%kind)

        if (allocated(this%name)) then
            call json%add(obj, 'name', this%name)
        end if

        if (allocated(this%operator)) then
            call json%add(obj, 'operator', this%operator)
        end if

        if (allocated(this%procedure_indices)) then
            block
                type(json_value), pointer :: procedures_array
                call json%create_array(procedures_array, 'procedure_indices')
                do i = 1, size(this%procedure_indices)
                    block
                        type(json_value), pointer :: proc_obj
                        call json%create_object(proc_obj, '')
                       call json%add(proc_obj, 'arena_index', this%procedure_indices(i))
                        call json%add(procedures_array, proc_obj)
                    end block
                end do
                call json%add(obj, procedures_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine interface_block_to_json

    subroutine module_to_json(this, json, parent)
        class(module_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'module')
        call json%add(obj, 'name', this%name)
        call json%add(obj, 'has_contains', this%has_contains)
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        ! Add declaration indices array
        if (allocated(this%declaration_indices)) then
            block
                type(json_value), pointer :: declarations_array
                call json%create_array(declarations_array, 'declaration_indices')
                do i = 1, size(this%declaration_indices)
                    block
                        type(json_value), pointer :: decl_obj
                        call json%create_object(decl_obj, '')
                     call json%add(decl_obj, 'arena_index', this%declaration_indices(i))
                        call json%add(declarations_array, decl_obj)
                    end block
                end do
                call json%add(obj, declarations_array)
            end block
        end if

        ! Add procedure indices array
        if (allocated(this%procedure_indices)) then
            block
                type(json_value), pointer :: procedures_array
                call json%create_array(procedures_array, 'procedure_indices')
                do i = 1, size(this%procedure_indices)
                    block
                        type(json_value), pointer :: proc_obj
                        call json%create_object(proc_obj, '')
                       call json%add(proc_obj, 'arena_index', this%procedure_indices(i))
                        call json%add(procedures_array, proc_obj)
                    end block
                end do
                call json%add(obj, procedures_array)
            end block
        end if

        call json%add(parent, obj)
    end subroutine module_to_json

    subroutine stop_to_json(this, json, parent)
        class(stop_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'stop')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (this%stop_code_index > 0) then
            call json%add(obj, 'stop_code_index', this%stop_code_index)
        end if

        if (allocated(this%stop_message)) then
            call json%add(obj, 'stop_message', this%stop_message)
        end if

        call json%add(parent, obj)
    end subroutine stop_to_json

    subroutine return_to_json(this, json, parent)
        class(return_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'return')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        call json%add(parent, obj)
    end subroutine return_to_json

    subroutine cycle_to_json(this, json, parent)
        class(cycle_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'cycle')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%loop_label)) then
            call json%add(obj, 'loop_label', this%loop_label)
        end if

        call json%add(parent, obj)
    end subroutine cycle_to_json

    subroutine exit_to_json(this, json, parent)
        class(exit_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'exit')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)

        if (allocated(this%loop_label)) then
            call json%add(obj, 'loop_label', this%loop_label)
        end if

        call json%add(parent, obj)
    end subroutine exit_to_json

    subroutine where_to_json(this, json, parent)
        class(where_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj
        integer :: i

        call json%create_object(obj, '')
        call json%add(obj, 'node_type', 'where')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'mask_expr_index', this%mask_expr_index)

        ! Add WHERE body indices
        if (allocated(this%where_body_indices)) then
            block
                type(json_value), pointer :: arr
                call json%create_array(arr, 'where_body_indices')
                do i = 1, size(this%where_body_indices)
                    call json%add(arr, '', this%where_body_indices(i))
                end do
                call json%add(obj, arr)
            end block
        end if

        ! Add ELSEWHERE body indices if present
        if (allocated(this%elsewhere_body_indices)) then
            block
                type(json_value), pointer :: arr
                call json%create_array(arr, 'elsewhere_body_indices')
                do i = 1, size(this%elsewhere_body_indices)
                    call json%add(arr, '', this%elsewhere_body_indices(i))
                end do
                call json%add(obj, arr)
            end block
        end if

        call json%add(parent, obj)
    end subroutine where_to_json

    ! High-performance arena memory management operations

    ! Shrink arena to optimal size (chunk-based memory management)
    subroutine shrink_arena(this)
        class(ast_arena_t), intent(inout) :: this
        type(ast_entry_t), allocatable :: temp_entries(:)
        integer :: new_capacity

        ! Calculate optimal new capacity (multiple of chunk_size)
        new_capacity = max(this%initial_capacity, &
                           ((this%size/this%chunk_size) + 1)*this%chunk_size)

        if (new_capacity < this%capacity) then
            allocate (temp_entries(new_capacity))
            if (this%size > 0) then
                temp_entries(1:this%size) = this%entries(1:this%size)
            end if
            this%entries = temp_entries
            this%capacity = new_capacity
        end if
    end subroutine shrink_arena

    ! Visitor pattern implementations (basic versions for core types)
    subroutine program_accept(this, visitor)
        class(program_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine program_accept

    subroutine assignment_accept(this, visitor)
        class(assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine assignment_accept

    subroutine pointer_assignment_accept(this, visitor)
        class(pointer_assignment_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine pointer_assignment_accept

    subroutine binary_op_accept(this, visitor)
        class(binary_op_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine binary_op_accept

    subroutine function_def_accept(this, visitor)
        class(function_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine function_def_accept

    subroutine subroutine_def_accept(this, visitor)
        class(subroutine_def_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine subroutine_def_accept

    subroutine subroutine_call_accept(this, visitor)
        class(subroutine_call_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine subroutine_call_accept

    subroutine call_or_subscript_accept(this, visitor)
        class(call_or_subscript_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine call_or_subscript_accept

    subroutine identifier_accept(this, visitor)
        class(identifier_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine identifier_accept

    subroutine literal_accept(this, visitor)
        class(literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine literal_accept

    subroutine use_statement_accept(this, visitor)
        class(use_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine use_statement_accept

    subroutine include_statement_accept(this, visitor)
        class(include_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine include_statement_accept

    subroutine print_statement_accept(this, visitor)
        class(print_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine print_statement_accept

    subroutine write_statement_accept(this, visitor)
        class(write_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine write_statement_accept

    subroutine read_statement_accept(this, visitor)
        class(read_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine read_statement_accept

    subroutine format_descriptor_accept(this, visitor)
        class(format_descriptor_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine format_descriptor_accept

    subroutine allocate_statement_accept(this, visitor)
        class(allocate_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine allocate_statement_accept

    subroutine deallocate_statement_accept(this, visitor)
        class(deallocate_statement_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine deallocate_statement_accept

    subroutine declaration_accept(this, visitor)
        class(declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine declaration_accept

    subroutine parameter_declaration_accept(this, visitor)
        class(parameter_declaration_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine parameter_declaration_accept

    subroutine do_loop_accept(this, visitor)
        class(do_loop_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine do_loop_accept

    subroutine do_while_accept(this, visitor)
        class(do_while_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine do_while_accept

    subroutine if_accept(this, visitor)
        class(if_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine if_accept

    subroutine derived_type_accept(this, visitor)
        class(derived_type_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine derived_type_accept

    subroutine interface_block_accept(this, visitor)
        class(interface_block_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine interface_block_accept

    subroutine module_accept(this, visitor)
        class(module_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine module_accept

    subroutine stop_accept(this, visitor)
        class(stop_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine stop_accept

    subroutine return_accept(this, visitor)
        class(return_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine return_accept

    subroutine cycle_accept(this, visitor)
        class(cycle_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine cycle_accept

    subroutine exit_accept(this, visitor)
        class(exit_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine exit_accept

    subroutine where_accept(this, visitor)
        class(where_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine where_accept

    ! Deep copy an AST entry
    function ast_entry_deep_copy(this) result(copy)
        class(ast_entry_t), intent(in) :: this
        type(ast_entry_t) :: copy
        integer :: i

        ! Copy scalar fields
        copy%parent_index = this%parent_index
        copy%depth = this%depth
        copy%child_count = this%child_count

        ! Deep copy allocatable components
        if (allocated(this%node_type)) then
            copy%node_type = this%node_type
        end if

        if (allocated(this%child_indices)) then
            allocate (copy%child_indices(size(this%child_indices)))
            copy%child_indices = this%child_indices
        end if

        ! Note: We cannot deep copy the polymorphic node component
        ! Deep copy the polymorphic node component using sourced allocation
        if (allocated(this%node)) then
            allocate(copy%node, source=this%node)
        end if
    end function ast_entry_deep_copy

    ! Assignment operator for ast_entry_t (deep copy)
    subroutine ast_entry_assign(lhs, rhs)
        class(ast_entry_t), intent(out) :: lhs
        type(ast_entry_t), intent(in) :: rhs
        integer :: i

        ! Copy scalar fields
        lhs%parent_index = rhs%parent_index
        lhs%depth = rhs%depth
        lhs%child_count = rhs%child_count

        ! Deep copy allocatable components
        if (allocated(rhs%node_type)) then
            lhs%node_type = rhs%node_type
        end if

        if (allocated(rhs%child_indices)) then
            allocate (lhs%child_indices(size(rhs%child_indices)))
            lhs%child_indices = rhs%child_indices
        end if

        ! Deep copy the node using sourced allocation
        if (allocated(rhs%node)) then
            allocate(lhs%node, source=rhs%node)
        end if
    end subroutine ast_entry_assign

    ! Deep copy an AST arena
    function ast_arena_deep_copy(this) result(copy)
        class(ast_arena_t), intent(in) :: this
        type(ast_arena_t) :: copy
        integer :: i

        ! Copy scalar fields
        copy%size = this%size
        copy%capacity = this%capacity
        copy%current_index = this%current_index
        copy%max_depth = this%max_depth
        copy%chunk_size = this%chunk_size
        copy%initial_capacity = this%initial_capacity

        ! Deep copy entries array
        if (allocated(this%entries)) then
            allocate (copy%entries(size(this%entries)))
            do i = 1, size(this%entries)
                copy%entries(i) = this%entries(i)  ! Uses ast_entry assignment
            end do
        end if
    end function ast_arena_deep_copy

    ! Assignment operator for ast_arena_t (deep copy)
    subroutine ast_arena_assign(lhs, rhs)
        class(ast_arena_t), intent(out) :: lhs
        type(ast_arena_t), intent(in) :: rhs
        integer :: i

        ! Copy scalar fields
        lhs%size = rhs%size
        lhs%capacity = rhs%capacity
        lhs%current_index = rhs%current_index
        lhs%max_depth = rhs%max_depth
        lhs%chunk_size = rhs%chunk_size
        lhs%initial_capacity = rhs%initial_capacity

        ! Deep copy entries array
        if (allocated(rhs%entries)) then
            allocate (lhs%entries(size(rhs%entries)))
            do i = 1, size(rhs%entries)
                lhs%entries(i) = rhs%entries(i)  ! Uses ast_entry assignment
            end do
        end if
    end subroutine ast_arena_assign

    ! Assignment operator for call_or_subscript_node (deep copy)
    subroutine call_or_subscript_assign(lhs, rhs)
        class(call_or_subscript_node), intent(out) :: lhs
        type(call_or_subscript_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Copy specific components
        lhs%name = rhs%name

        ! Deep copy allocatable array
        if (allocated(rhs%arg_indices)) then
            allocate (lhs%arg_indices(size(rhs%arg_indices)))
            lhs%arg_indices = rhs%arg_indices
        end if
    end subroutine call_or_subscript_assign

    ! Assignment operator for array_literal_node (deep copy)
    subroutine array_literal_assign(lhs, rhs)
        class(array_literal_node), intent(out) :: lhs
        type(array_literal_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Deep copy allocatable array
        if (allocated(rhs%element_indices)) then
            allocate (lhs%element_indices(size(rhs%element_indices)))
            lhs%element_indices = rhs%element_indices
        end if
    end subroutine array_literal_assign

    ! Complex literal procedures
    subroutine complex_literal_accept(this, visitor)
        class(complex_literal_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Basic accept implementation - can be overridden
    end subroutine complex_literal_accept

    subroutine complex_literal_to_json(this, json, parent)
        class(complex_literal_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'complex_literal')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'real_index', this%real_index)
        call json%add(obj, 'imag_index', this%imag_index)
        call json%add(parent, obj)
    end subroutine complex_literal_to_json

    ! Assignment operator for complex_literal_node (deep copy)
    subroutine complex_literal_assign(lhs, rhs)
        class(complex_literal_node), intent(out) :: lhs
        type(complex_literal_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            lhs%inferred_type = rhs%inferred_type
        end if

        ! Copy complex literal specific components
        lhs%real_index = rhs%real_index
        lhs%imag_index = rhs%imag_index
    end subroutine complex_literal_assign

    ! Assignment operator for do_loop_node (deep copy)
    subroutine do_loop_assign(lhs, rhs)
        class(do_loop_node), intent(out) :: lhs
        type(do_loop_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Copy specific components
        lhs%var_name = rhs%var_name
        if (allocated(rhs%label)) lhs%label = rhs%label
        lhs%start_expr_index = rhs%start_expr_index
        lhs%end_expr_index = rhs%end_expr_index
        lhs%step_expr_index = rhs%step_expr_index

        ! Deep copy allocatable array
        if (allocated(rhs%body_indices)) then
            allocate (lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine do_loop_assign

    ! JSON serialization for forall_node
    subroutine forall_to_json(this, json, parent)
        class(forall_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'forall')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'index_var', this%index_var)
        call json%add(obj, 'start_index', this%start_index)
        call json%add(obj, 'end_index', this%end_index)
        if (this%step_index > 0) call json%add(obj, 'step_index', this%step_index)
        if (this%mask_index > 0) call json%add(obj, 'mask_index', this%mask_index)
        call json%add(parent, obj)
    end subroutine forall_to_json

    ! Assignment operator for forall_node (deep copy)
    subroutine forall_assign(lhs, rhs)
        class(forall_node), intent(out) :: lhs
        type(forall_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Copy specific components
        lhs%index_var = rhs%index_var
        lhs%start_index = rhs%start_index
        lhs%end_index = rhs%end_index
        lhs%step_index = rhs%step_index
        lhs%mask_index = rhs%mask_index

        ! Deep copy allocatable array
        if (allocated(rhs%body_indices)) then
            allocate (lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine forall_assign

    ! Visitor accept for forall_node (stub implementation)
    subroutine forall_accept(this, visitor)
        class(forall_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation - visitor pattern not fully implemented yet
    end subroutine forall_accept

    ! JSON serialization for select_case_node
    subroutine select_case_to_json(this, json, parent)
        class(select_case_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'select_case')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'selector_index', this%selector_index)
     if (this%default_index > 0) call json%add(obj, 'default_index', this%default_index)
        call json%add(parent, obj)
    end subroutine select_case_to_json

    ! Assignment operator for select_case_node (deep copy)
    subroutine select_case_assign(lhs, rhs)
        class(select_case_node), intent(out) :: lhs
        type(select_case_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Copy specific components
        lhs%selector_index = rhs%selector_index
        lhs%default_index = rhs%default_index

        ! Deep copy allocatable array
        if (allocated(rhs%case_indices)) then
            allocate (lhs%case_indices(size(rhs%case_indices)))
            lhs%case_indices = rhs%case_indices
        end if
    end subroutine select_case_assign

    ! Visitor accept for select_case_node (stub implementation)
    subroutine select_case_accept(this, visitor)
        class(select_case_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation - visitor pattern not fully implemented yet
    end subroutine select_case_accept

    ! JSON serialization for case_block_node
    subroutine case_block_to_json(this, json, parent)
        class(case_block_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'case_block')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine case_block_to_json

    ! Assignment operator for case_block_node (deep copy)
    subroutine case_block_assign(lhs, rhs)
        class(case_block_node), intent(out) :: lhs
        type(case_block_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Deep copy allocatable arrays
        if (allocated(rhs%value_indices)) then
            allocate (lhs%value_indices(size(rhs%value_indices)))
            lhs%value_indices = rhs%value_indices
        end if
        if (allocated(rhs%body_indices)) then
            allocate (lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine case_block_assign

    ! Visitor accept for case_block_node (stub implementation)
    subroutine case_block_accept(this, visitor)
        class(case_block_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation - visitor pattern not fully implemented yet
    end subroutine case_block_accept

    ! JSON serialization for case_range_node
    subroutine case_range_to_json(this, json, parent)
        class(case_range_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'case_range')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(obj, 'start_value', this%start_value)
        call json%add(obj, 'end_value', this%end_value)
        call json%add(parent, obj)
    end subroutine case_range_to_json

    ! Assignment operator for case_range_node (deep copy)
    subroutine case_range_assign(lhs, rhs)
        class(case_range_node), intent(out) :: lhs
        type(case_range_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Copy specific components
        lhs%start_value = rhs%start_value
        lhs%end_value = rhs%end_value
    end subroutine case_range_assign

    ! Visitor accept for case_range_node (stub implementation)
    subroutine case_range_accept(this, visitor)
        class(case_range_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation - visitor pattern not fully implemented yet
    end subroutine case_range_accept

    ! JSON serialization for case_default_node
    subroutine case_default_to_json(this, json, parent)
        class(case_default_node), intent(in) :: this
        type(json_core), intent(inout) :: json
        type(json_value), pointer, intent(in) :: parent
        type(json_value), pointer :: obj

        call json%create_object(obj, '')
        call json%add(obj, 'type', 'case_default')
        call json%add(obj, 'line', this%line)
        call json%add(obj, 'column', this%column)
        call json%add(parent, obj)
    end subroutine case_default_to_json

    ! Assignment operator for case_default_node (deep copy)
    subroutine case_default_assign(lhs, rhs)
        class(case_default_node), intent(out) :: lhs
        type(case_default_node), intent(in) :: rhs

        ! Copy base class components
        lhs%line = rhs%line
        lhs%column = rhs%column
        if (allocated(rhs%inferred_type)) then
            allocate (lhs%inferred_type)
            lhs%inferred_type = rhs%inferred_type  ! Use assignment for deep copy
        end if

        ! Deep copy allocatable array
        if (allocated(rhs%body_indices)) then
            allocate (lhs%body_indices(size(rhs%body_indices)))
            lhs%body_indices = rhs%body_indices
        end if
    end subroutine case_default_assign

    ! Visitor accept for case_default_node (stub implementation)
    subroutine case_default_accept(this, visitor)
        class(case_default_node), intent(in) :: this
        class(*), intent(inout) :: visitor
        ! Stub implementation - visitor pattern not fully implemented yet
    end subroutine case_default_accept

end module ast_core
