module variable_collection
    ! Unified variable collection utilities
    ! Consolidates DRY violations from multiple collection implementations
    use ast_core
    use type_system_arena, only: mono_handle_t, null_mono_handle
    implicit none
    private

    ! Public types and interfaces
    public :: variable_info_t, variable_collection_t
    public :: create_variable_collection, collect_variables
    public :: collect_identifiers, collect_typed_variables
    public :: collection_filter_t, FILTER_ALL, FILTER_DECLARED, FILTER_UNDECLARED

    ! Collection filter types
    integer, parameter :: FILTER_ALL = 0
    integer, parameter :: FILTER_DECLARED = 1
    integer, parameter :: FILTER_UNDECLARED = 2

    ! Variable information
    type :: variable_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: type_name
        logical :: is_declared = .false.
        logical :: needs_allocatable = .false.
        integer :: usage_count = 0
        integer :: node_index = 0
        type(mono_handle_t) :: inferred_type
    contains
        procedure :: assign => variable_info_assign
        generic :: assignment(=) => assign
    end type variable_info_t

    ! Collection container
    type :: variable_collection_t
        type(variable_info_t), allocatable :: variables(:)
        integer :: count = 0
        integer :: capacity = 0
    contains
        procedure :: add_variable => collection_add_variable
        procedure :: find_variable => collection_find_variable
        procedure :: has_variable => collection_has_variable
        procedure :: resize => collection_resize
        procedure :: clear => collection_clear
        procedure :: assign => variable_collection_assign
        generic :: assignment(=) => assign
        final :: collection_finalize
    end type variable_collection_t

    ! Filter function interface
    abstract interface
        logical function collection_filter_t(var_info)
            import :: variable_info_t
            type(variable_info_t), intent(in) :: var_info
        end function collection_filter_t
    end interface

contains

    ! Create a new variable collection
    function create_variable_collection(initial_capacity) result(collection)
        integer, intent(in), optional :: initial_capacity
        type(variable_collection_t) :: collection
        integer :: capacity

        capacity = 100
        if (present(initial_capacity)) capacity = initial_capacity

        collection%capacity = capacity
        collection%count = 0
        allocate(collection%variables(capacity))
    end function create_variable_collection

    ! Main unified variable collection function
    subroutine collect_variables(arena, root_index, collection, filter_type, &
                                function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        type(variable_collection_t), intent(inout) :: collection
        integer, intent(in), optional :: filter_type
        character(len=*), intent(in), optional :: function_names(:)
        integer, intent(in), optional :: func_count
        
        integer :: filter_mode
        character(len=64), allocatable :: func_names(:)
        integer :: num_funcs
        
        filter_mode = FILTER_ALL
        if (present(filter_type)) filter_mode = filter_type
        
        num_funcs = 0
        if (present(function_names) .and. present(func_count)) then
            num_funcs = func_count
            allocate(func_names(num_funcs))
            func_names(1:num_funcs) = function_names(1:num_funcs)
        else
            allocate(func_names(0))
        end if
        
        call collect_variables_recursive(arena, root_index, collection, &
                                       filter_mode, func_names, num_funcs)
    end subroutine collect_variables

    ! Simplified interface for identifier collection
    subroutine collect_identifiers(arena, root_index, names, count, max_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=*), intent(inout) :: names(:)
        integer, intent(inout) :: count
        integer, intent(in) :: max_count
        
        type(variable_collection_t) :: collection
        integer :: i
        
        collection = create_variable_collection()
        call collect_variables(arena, root_index, collection)
        
        count = min(collection%count, max_count)
        do i = 1, count
            names(i) = collection%variables(i)%name
        end do
    end subroutine collect_identifiers

    ! Simplified interface for typed variable collection  
    subroutine collect_typed_variables(arena, root_index, var_names, var_types, &
                                     var_declared, var_count, max_count, &
                                     function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: root_index
        character(len=*), intent(inout) :: var_names(:)
        character(len=*), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        integer, intent(in) :: max_count
        character(len=*), intent(in), optional :: function_names(:)
        integer, intent(in), optional :: func_count
        
        type(variable_collection_t) :: collection
        integer :: i
        
        collection = create_variable_collection()
        call collect_variables(arena, root_index, collection, FILTER_ALL, &
                             function_names, func_count)
        
        var_count = min(collection%count, max_count)
        do i = 1, var_count
            var_names(i) = collection%variables(i)%name
            var_types(i) = collection%variables(i)%type_name
            var_declared(i) = collection%variables(i)%is_declared
        end do
    end subroutine collect_typed_variables

    ! Core recursive collection implementation
    recursive subroutine collect_variables_recursive(arena, node_index, collection, &
                                                    filter_type, function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(variable_collection_t), intent(inout) :: collection
        integer, intent(in) :: filter_type
        character(len=*), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i

        if (node_index <= 0 .or. node_index > arena%size) return
        if (.not. allocated(arena%entries(node_index)%node)) return

        select type (node => arena%entries(node_index)%node)
        type is (identifier_node)
            call process_identifier_node(node, collection, function_names, func_count)
            
        type is (assignment_node)
            ! Process target variable
            call collect_variables_recursive(arena, node%target_index, collection, &
                                           filter_type, function_names, func_count)
            ! Process value expression
            call collect_variables_recursive(arena, node%value_index, collection, &
                                           filter_type, function_names, func_count)
            
        type is (declaration_node)
            call process_declaration_node(node, collection, filter_type)
            
        type is (binary_op_node)
            call collect_variables_recursive(arena, node%left_index, collection, &
                                           filter_type, function_names, func_count)
            call collect_variables_recursive(arena, node%right_index, collection, &
                                           filter_type, function_names, func_count)
            
        type is (call_or_subscript_node)
            ! Check if this is a function call (not a variable)
            if (.not. is_function_name(node%name, function_names, func_count)) then
                call process_identifier_variable(node%name, "", collection, &
                                                function_names, func_count)
            end if
            ! Process arguments
            if (allocated(node%arg_indices)) then
                do i = 1, size(node%arg_indices)
                    call collect_variables_recursive(arena, node%arg_indices(i), &
                                                   collection, filter_type, &
                                                   function_names, func_count)
                end do
            end if
            
        type is (do_loop_node)
            ! Add loop variable
            call process_identifier_variable(node%var_name, "integer", collection, &
                                           function_names, func_count)
            ! Process bounds
            call collect_variables_recursive(arena, node%start_expr_index, collection, &
                                           filter_type, function_names, func_count)
            call collect_variables_recursive(arena, node%end_expr_index, collection, &
                                           filter_type, function_names, func_count)
            if (node%step_expr_index > 0) then
                call collect_variables_recursive(arena, node%step_expr_index, &
                                               collection, filter_type, &
                                               function_names, func_count)
            end if
            ! Process body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_variables_recursive(arena, node%body_indices(i), &
                                                   collection, filter_type, &
                                                   function_names, func_count)
                end do
            end if
            
        type is (do_while_node)
            ! Process condition
            call collect_variables_recursive(arena, node%condition_index, collection, &
                                           filter_type, function_names, func_count)
            ! Process body
            if (allocated(node%body_indices)) then
                do i = 1, size(node%body_indices)
                    call collect_variables_recursive(arena, node%body_indices(i), &
                                                   collection, filter_type, &
                                                   function_names, func_count)
                end do
            end if
            
        type is (if_node)
            ! Process condition
            call collect_variables_recursive(arena, node%condition_index, collection, &
                                           filter_type, function_names, func_count)
            ! Process then body
            if (allocated(node%then_body_indices)) then
                do i = 1, size(node%then_body_indices)
                    call collect_variables_recursive(arena, node%then_body_indices(i), &
                                                   collection, filter_type, &
                                                   function_names, func_count)
                end do
            end if
            ! Process else body
            if (allocated(node%else_body_indices)) then
                do i = 1, size(node%else_body_indices)
                    call collect_variables_recursive(arena, node%else_body_indices(i), &
                                                   collection, filter_type, &
                                                   function_names, func_count)
                end do
            end if
            
        class default
            ! For other node types, try to traverse known child patterns
            ! This is a fallback for node types not explicitly handled
            continue
        end select
    end subroutine collect_variables_recursive

    ! Process identifier node
    subroutine process_identifier_node(node, collection, function_names, func_count)
        type(identifier_node), intent(in) :: node
        type(variable_collection_t), intent(inout) :: collection
        character(len=*), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        
        call process_identifier_variable(node%name, "", collection, &
                                       function_names, func_count)
    end subroutine process_identifier_node

    ! Process declaration node
    subroutine process_declaration_node(node, collection, filter_type)
        type(declaration_node), intent(in) :: node
        type(variable_collection_t), intent(inout) :: collection
        integer, intent(in) :: filter_type
        type(variable_info_t) :: var_info
        integer :: i
        
        if (node%is_multi_declaration .and. allocated(node%var_names)) then
            ! Handle multi-variable declaration
            do i = 1, size(node%var_names)
                var_info%name = node%var_names(i)
                var_info%type_name = node%type_name
                var_info%is_declared = .true.
                var_info%needs_allocatable = node%is_allocatable
                call collection%add_variable(var_info)
            end do
        else
            ! Handle single variable declaration
            var_info%name = node%var_name
            var_info%type_name = node%type_name
            var_info%is_declared = .true.
            var_info%needs_allocatable = node%is_allocatable
            call collection%add_variable(var_info)
        end if
    end subroutine process_declaration_node

    ! Process identifier as variable
    subroutine process_identifier_variable(name, type_hint, collection, &
                                         function_names, func_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: type_hint
        type(variable_collection_t), intent(inout) :: collection
        character(len=*), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        type(variable_info_t) :: var_info
        integer :: existing_index
        
        ! Skip function names
        if (is_function_name(name, function_names, func_count)) return
        
        ! Check if variable already exists
        existing_index = collection%find_variable(name)
        
        if (existing_index > 0) then
            ! Update existing variable
            collection%variables(existing_index)%usage_count = &
                collection%variables(existing_index)%usage_count + 1
            if (len_trim(type_hint) > 0 .and. &
                len_trim(collection%variables(existing_index)%type_name) == 0) then
                collection%variables(existing_index)%type_name = type_hint
            end if
        else
            ! Add new variable
            var_info%name = name
            var_info%type_name = type_hint
            var_info%is_declared = .false.
            var_info%usage_count = 1
            call collection%add_variable(var_info)
        end if
    end subroutine process_identifier_variable

    ! Check if a name is a function name
    logical function is_function_name(name, function_names, func_count)
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        integer :: i
        
        is_function_name = .false.
        do i = 1, func_count
            if (trim(name) == trim(function_names(i))) then
                is_function_name = .true.
                return
            end if
        end do
    end function is_function_name

    ! Collection methods
    subroutine collection_add_variable(this, var_info)
        class(variable_collection_t), intent(inout) :: this
        type(variable_info_t), intent(in) :: var_info
        
        if (this%count >= this%capacity) then
            call this%resize(this%capacity * 2)
        end if
        
        this%count = this%count + 1
        this%variables(this%count) = var_info
    end subroutine collection_add_variable

    function collection_find_variable(this, name) result(index)
        class(variable_collection_t), intent(in) :: this
        character(len=*), intent(in) :: name
        integer :: index
        integer :: i
        
        index = 0
        do i = 1, this%count
            if (trim(this%variables(i)%name) == trim(name)) then
                index = i
                return
            end if
        end do
    end function collection_find_variable

    logical function collection_has_variable(this, name)
        class(variable_collection_t), intent(in) :: this
        character(len=*), intent(in) :: name
        
        collection_has_variable = (this%find_variable(name) > 0)
    end function collection_has_variable

    subroutine collection_resize(this, new_capacity)
        class(variable_collection_t), intent(inout) :: this
        integer, intent(in) :: new_capacity
        type(variable_info_t), allocatable :: temp_vars(:)
        integer :: old_count
        
        if (new_capacity <= this%capacity) return
        
        old_count = this%count
        allocate(temp_vars(old_count))
        temp_vars(1:old_count) = this%variables(1:old_count)
        
        deallocate(this%variables)
        allocate(this%variables(new_capacity))
        this%variables(1:old_count) = temp_vars(1:old_count)
        this%capacity = new_capacity
    end subroutine collection_resize

    subroutine collection_clear(this)
        class(variable_collection_t), intent(inout) :: this
        this%count = 0
    end subroutine collection_clear

    subroutine collection_finalize(this)
        type(variable_collection_t), intent(inout) :: this
        if (allocated(this%variables)) deallocate(this%variables)
        this%count = 0
        this%capacity = 0
    end subroutine collection_finalize

    ! Deep copy assignment operator for variable_info_t
    subroutine variable_info_assign(lhs, rhs)
        class(variable_info_t), intent(inout) :: lhs
        type(variable_info_t), intent(in) :: rhs
        logical :: is_same
        
        ! Check for self-assignment
        is_same = .false.
        if (allocated(lhs%name) .and. allocated(rhs%name)) then
            if (lhs%name == rhs%name .and. &
                lhs%is_declared .eqv. rhs%is_declared .and. &
                lhs%usage_count == rhs%usage_count .and. &
                lhs%node_index == rhs%node_index) then
                is_same = .true.
            end if
        end if
        if (is_same) return
        
        ! Clear existing allocatable components
        if (allocated(lhs%name)) deallocate(lhs%name)
        if (allocated(lhs%type_name)) deallocate(lhs%type_name)
        
        ! Copy scalar fields
        lhs%is_declared = rhs%is_declared
        lhs%needs_allocatable = rhs%needs_allocatable
        lhs%usage_count = rhs%usage_count
        lhs%node_index = rhs%node_index
        
        ! Deep copy allocatable character strings
        if (allocated(rhs%name)) then
            lhs%name = rhs%name
        end if
        
        if (allocated(rhs%type_name)) then
            lhs%type_name = rhs%type_name
        end if
        
        ! Copy arena handle directly (no allocation needed)
        lhs%inferred_type = rhs%inferred_type
    end subroutine variable_info_assign

    ! Deep copy assignment operator for variable_collection_t
    subroutine variable_collection_assign(lhs, rhs)
        class(variable_collection_t), intent(inout) :: lhs
        type(variable_collection_t), intent(in) :: rhs
        integer :: i
        logical :: is_same
        
        ! Check for self-assignment
        is_same = .false.
        if (allocated(lhs%variables) .and. allocated(rhs%variables)) then
            if (size(lhs%variables) == size(rhs%variables) .and. &
                lhs%count == rhs%count .and. &
                lhs%capacity == rhs%capacity) then
                is_same = .true.
            end if
        end if
        if (is_same) return
        
        ! Clear existing allocatable components
        if (allocated(lhs%variables)) deallocate(lhs%variables)
        
        ! Copy scalar fields
        lhs%count = rhs%count
        lhs%capacity = rhs%capacity
        
        ! Deep copy allocatable array of variables
        if (allocated(rhs%variables)) then
            allocate(lhs%variables(size(rhs%variables)))
            do i = 1, size(rhs%variables)
                ! This uses variable_info_t assignment operator (deep copy)
                lhs%variables(i) = rhs%variables(i)
            end do
        end if
    end subroutine variable_collection_assign

end module variable_collection