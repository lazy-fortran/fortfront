module codegen_utilities
    use iso_fortran_env, only: error_unit
    use ast_core
    use ast_nodes_data, only: intent_type_to_string, INTENT_NONE
    use type_system_unified
    use string_types, only: string_t
    use codegen_indent
    implicit none
    private

    ! Type standardization configuration
    logical, save :: standardize_types_enabled = .true.

    ! Context for executable code before contains
    logical, save :: context_has_executable_before_contains = .false.

    public :: set_type_standardization, get_type_standardization
    public :: int_to_string
    public :: find_node_index_in_arena
    public :: same_node
    public :: can_group_declarations
    public :: can_group_parameters
    public :: can_group_declarations_with_params
    public :: build_param_name_with_dims
    public :: generate_grouped_declaration
    public :: generate_grouped_body
    public :: generate_grouped_body_with_params
    public :: generate_grouped_body_context
    public :: find_parameter_info
    public :: is_function_parameter
    public :: is_parameter_name
    public :: add_line_continuations
    public :: add_line_with_continuation
    
    ! Interface for calling back to main code generator
    interface
        function generate_code_from_arena(arena, node_index) result(code)
            import :: ast_arena_t
            type(ast_arena_t), intent(in) :: arena
            integer, intent(in) :: node_index
            character(len=:), allocatable :: code
        end function generate_code_from_arena
    end interface
    
    ! Type for storing parameter information during codegen
    type, public :: parameter_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: intent_str
        logical :: is_optional
    end type parameter_info_t

contains

    ! Set type standardization flag
    subroutine set_type_standardization(enabled)
        logical, intent(in) :: enabled
        standardize_types_enabled = enabled
    end subroutine set_type_standardization

    ! Get type standardization flag
    subroutine get_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardize_types_enabled
    end subroutine get_type_standardization

    ! Convert integer to string
    function int_to_string(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=32) :: buffer
        write(buffer, '(I0)') num
        str = trim(buffer)
    end function int_to_string

    ! Find node index in arena
    function find_node_index_in_arena(arena, target_node) result(index)
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: target_node
        integer :: index
        integer :: i

        index = 0
        do i = 1, arena%size
            if (allocated(arena%entries(i)%node)) then
                if (same_node(arena%entries(i)%node, target_node)) then
                    index = i
                    return
                end if
            end if
        end do
    end function find_node_index_in_arena

    ! Check if two nodes are the same
    function same_node(node1, node2) result(is_same)
        class(ast_node), intent(in) :: node1, node2
        logical :: is_same

        is_same = .false.

        select type (n1 => node1)
        type is (assignment_node)
            select type (n2 => node2)
            type is (assignment_node)
                ! Assignment nodes don't have direct left/right members in the current implementation
                ! They have indices instead
                is_same = .false.
            end select
        type is (identifier_node)
            select type (n2 => node2)
            type is (identifier_node)
                is_same = (n1%name == n2%name)
            end select
        type is (literal_node)
            select type (n2 => node2)
            type is (literal_node)
                is_same = (n1%literal_type == n2%literal_type)
            end select
        end select
    end function same_node

    ! Check if two declarations can be grouped
    function can_group_declarations(node1, node2) result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        logical :: can_group

        ! Don't group declarations that have initializers
        if (node1%initializer_index > 0 .or. node2%initializer_index > 0) then
            can_group = .false.
            return
        end if

        can_group = trim(node1%type_name) == trim(node2%type_name) .and. &
                    node1%kind_value == node2%kind_value .and. &
                    node1%has_kind .eqv. node2%has_kind .and. &
                    ((node1%has_intent .and. node2%has_intent .and. &
                      trim(node1%intent) == trim(node2%intent)) .or. &
                     (.not. node1%has_intent .and. .not. node2%has_intent)) .and. &
                    node1%is_optional .eqv. node2%is_optional
    end function can_group_declarations

    ! Check if two parameter declarations can be grouped
    function can_group_parameters(node1, node2) result(can_group)
        type(parameter_declaration_node), intent(in) :: node1, node2
        logical :: can_group

        ! Use type_name instead of type_spec for parameter_declaration_node
        can_group = .true.
        if (allocated(node1%type_name) .and. allocated(node2%type_name)) then
            can_group = (trim(node1%type_name) == trim(node2%type_name))
        end if
        can_group = can_group .and. &
                    (node1%intent_type == node2%intent_type) .and. &
                    (node1%is_optional .eqv. node2%is_optional)
    end function can_group_parameters

    ! Check if declarations can be grouped considering parameter mapping
    function can_group_declarations_with_params(node1, node2, param_map) result(can_group)
        type(declaration_node), intent(in) :: node1, node2
        type(parameter_info_t), intent(in) :: param_map(:)
        logical :: can_group
        integer :: idx1, idx2
        character(len=:), allocatable :: intent1, intent2
        logical :: optional1, optional2

        ! Don't group declarations that have initializers
        if (node1%initializer_index > 0 .or. node2%initializer_index > 0) then
            can_group = .false.
            return
        end if

        ! Get intent and optional from param_map if these are parameters
        idx1 = find_parameter_info(param_map, node1%var_name)
        idx2 = find_parameter_info(param_map, node2%var_name)

        if (idx1 > 0) then
            intent1 = param_map(idx1)%intent_str
            optional1 = param_map(idx1)%is_optional
        else
            if (node1%has_intent) then
                intent1 = node1%intent
            else
                intent1 = ""
            end if
            optional1 = node1%is_optional
        end if

        if (idx2 > 0) then
            intent2 = param_map(idx2)%intent_str
            optional2 = param_map(idx2)%is_optional
        else
            if (node2%has_intent) then
                intent2 = node2%intent
            else
                intent2 = ""
            end if
            optional2 = node2%is_optional
        end if

        can_group = trim(node1%type_name) == trim(node2%type_name) .and. &
                    node1%kind_value == node2%kind_value .and. &
                    node1%has_kind .eqv. node2%has_kind .and. &
                    trim(intent1) == trim(intent2) .and. &
                    optional1 .eqv. optional2
    end function can_group_declarations_with_params

    ! Build parameter name with dimensions
    function build_param_name_with_dims(arena, param_node) result(name_with_dims)
        type(ast_arena_t), intent(in) :: arena
        type(parameter_declaration_node), intent(in) :: param_node
        character(len=:), allocatable :: name_with_dims
        integer :: d
        character(len=:), allocatable :: dim_code

        name_with_dims = param_node%name
        ! Note: parameter_declaration_node doesn't have dimension_indices in current implementation
        ! This is a placeholder for future enhancement
    end function build_param_name_with_dims

    ! Generate grouped declaration statement
    function generate_grouped_declaration(type_name, kind_value, has_kind, &
                                          intent, var_list, is_optional) result(stmt)
        character(len=*), intent(in) :: type_name
        integer, intent(in) :: kind_value
        logical, intent(in) :: has_kind
        character(len=*), intent(in) :: intent
        character(len=*), intent(in) :: var_list
        logical, intent(in), optional :: is_optional
        character(len=:), allocatable :: stmt
        logical :: opt_flag

        opt_flag = .false.
        if (present(is_optional)) opt_flag = is_optional

        stmt = type_name
        if (has_kind) then
            stmt = stmt // "(" // trim(adjustl(int_to_string(kind_value))) // ")"
        end if
        if (len_trim(intent) > 0) then
            stmt = stmt // ", intent(" // intent // ")"
        end if
        if (opt_flag) then
            stmt = stmt // ", optional"
        end if
        stmt = stmt // " :: " // var_list
    end function generate_grouped_declaration

    ! Generate grouped body statements
    function generate_grouped_body(arena, body_indices, indent) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        character(len=:), allocatable :: code
        character(len=:), allocatable :: stmt_code
        character(len=:), allocatable :: indent_str
        integer :: i, j
        logical :: in_contains_section
        logical :: found_contains

        ! Build indent string based on indent level
        indent_str = repeat("    ", indent)
        
        code = ""
        in_contains_section = .false.
        i = 1

        do while (i <= size(body_indices))
            if (body_indices(i) > 0 .and. body_indices(i) <= arena%size) then
                if (allocated(arena%entries(body_indices(i))%node)) then
                    select type (node => arena%entries(body_indices(i))%node)
                    type is (contains_node)
                        in_contains_section = .true.
                        code = code // "contains" // new_line('A')
                        i = i + 1
                        
                    type is (end_statement_node)
                        ! Skip end statements, they're handled by parent
                        i = i + 1
                        
                    type is (function_def_node)
                        if (in_contains_section .and. i > 1) then
                            code = code // new_line('A')  ! Extra blank line between procedures
                        end if
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_str // stmt_code // new_line('A')
                        i = i + 1
                        
                    type is (subroutine_def_node)
                        if (in_contains_section .and. i > 1) then
                            code = code // new_line('A')  ! Extra blank line between procedures
                        end if
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_str // stmt_code // new_line('A')
                        i = i + 1
                        
                    type is (declaration_node)
                        ! Group consecutive declarations of the same type
                        if (.not. in_contains_section .and. node%initializer_index == 0) then
                            call process_grouped_declarations(arena, body_indices, i, indent_str, code)
                        else
                            stmt_code = generate_code_from_arena(arena, body_indices(i))
                            code = code // indent_str // stmt_code // new_line('A')
                            i = i + 1
                        end if
                        
                    type is (parameter_declaration_node)
                        ! Group consecutive parameter declarations
                        call process_grouped_parameters(arena, body_indices, i, indent_str, code)
                        
                    type is (comment_node)
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        ! Comments preserve their own indentation
                        code = code // stmt_code // new_line('A')
                        i = i + 1
                        
                    type is (blank_line_node)
                        code = code // new_line('A')
                        i = i + 1
                        
                    class default
                        stmt_code = generate_code_from_arena(arena, body_indices(i))
                        code = code // indent_str // stmt_code // new_line('A')
                        i = i + 1
                    end select
                else
                    i = i + 1
                end if
            else
                i = i + 1
            end if
        end do
    end function generate_grouped_body

    ! Generate grouped body with parameter mapping
    function generate_grouped_body_with_params(arena, body_indices, indent, param_map, proc_node) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        type(parameter_info_t), intent(in) :: param_map(:)
        class(ast_node), intent(in) :: proc_node
        character(len=:), allocatable :: code
        
        ! For now, delegate to regular grouped body
        ! Full parameter-aware grouping would be implemented here
        code = generate_grouped_body(arena, body_indices, indent)
    end function generate_grouped_body_with_params

    ! Generate grouped body with context about executable statements
    function generate_grouped_body_context(arena, body_indices, indent, has_exec_before_contains) result(code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(in) :: indent
        logical, intent(in) :: has_exec_before_contains
        character(len=:), allocatable :: code
        
        ! Store context
        context_has_executable_before_contains = has_exec_before_contains
        
        ! Generate body
        code = generate_grouped_body(arena, body_indices, indent)
        
        ! Reset context
        context_has_executable_before_contains = .false.
    end function generate_grouped_body_context

    ! Process grouped declarations
    subroutine process_grouped_declarations(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code
        
        type(declaration_node) :: first_node
        character(len=:), allocatable :: var_list, stmt_code
        integer :: j
        
        select type (node => arena%entries(body_indices(i))%node)
        type is (declaration_node)
            first_node = node
            var_list = trim(node%var_name)
            
            ! Look ahead for groupable declarations
            j = i + 1
            do while (j <= size(body_indices))
                if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                    if (allocated(arena%entries(body_indices(j))%node)) then
                        select type (next_node => arena%entries(body_indices(j))%node)
                        type is (declaration_node)
                            if (can_group_declarations(first_node, next_node)) then
                                var_list = var_list // ", " // trim(next_node%var_name)
                                j = j + 1
                            else
                                exit
                            end if
                        class default
                            exit
                        end select
                    else
                        exit
                    end if
                else
                    exit
                end if
            end do
            
            ! Generate grouped declaration
            stmt_code = generate_grouped_declaration(first_node%type_name, &
                first_node%kind_value, first_node%has_kind, &
                merge(first_node%intent, "", first_node%has_intent), &
                var_list, first_node%is_optional)
            code = code // indent_str // stmt_code // new_line('A')
            i = j
        end select
    end subroutine process_grouped_declarations

    ! Process grouped parameter declarations
    subroutine process_grouped_parameters(arena, body_indices, i, indent_str, code)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        integer, intent(inout) :: i
        character(len=*), intent(in) :: indent_str
        character(len=:), allocatable, intent(inout) :: code
        
        type(parameter_declaration_node) :: first_node
        character(len=:), allocatable :: var_list, stmt_code
        integer :: j
        
        select type (node => arena%entries(body_indices(i))%node)
        type is (parameter_declaration_node)
            first_node = node
            var_list = trim(node%name)
            
            ! Look ahead for groupable parameter declarations
            j = i + 1
            do while (j <= size(body_indices))
                if (body_indices(j) > 0 .and. body_indices(j) <= arena%size) then
                    if (allocated(arena%entries(body_indices(j))%node)) then
                        select type (next_node => arena%entries(body_indices(j))%node)
                        type is (parameter_declaration_node)
                            if (can_group_parameters(first_node, next_node)) then
                                var_list = var_list // ", " // trim(next_node%name)
                                j = j + 1
                            else
                                exit
                            end if
                        class default
                            exit
                        end select
                    else
                        exit
                    end if
                else
                    exit
                end if
            end do
            
            ! Generate grouped parameter declaration
            if (allocated(first_node%type_name)) then
                stmt_code = first_node%type_name
            else
                stmt_code = "real"
            end if
            if (first_node%intent_type /= INTENT_NONE) then
                stmt_code = stmt_code // ", intent(" // intent_type_to_string(first_node%intent_type) // ")"
            end if
            if (first_node%is_optional) then
                stmt_code = stmt_code // ", optional"
            end if
            stmt_code = stmt_code // " :: " // var_list
            code = code // indent_str // stmt_code // new_line('A')
            i = j
        end select
    end subroutine process_grouped_parameters

    ! Find parameter information by name
    function find_parameter_info(param_map, var_name) result(param_idx)
        type(parameter_info_t), intent(in) :: param_map(:)
        character(len=*), intent(in) :: var_name
        integer :: param_idx
        integer :: i
        
        param_idx = 0
        do i = 1, size(param_map)
            if (allocated(param_map(i)%name)) then
                if (param_map(i)%name == var_name) then
                    param_idx = i
                    return
                end if
            end if
        end do
    end function find_parameter_info

    ! Check if a variable name is a function parameter
    function is_function_parameter(var_name, arena, proc_node) result(is_param)
        character(len=*), intent(in) :: var_name
        type(ast_arena_t), intent(in) :: arena
        class(ast_node), intent(in) :: proc_node
        logical :: is_param
        integer :: i
        
        is_param = .false.
        
        select type (proc_node)
        type is (function_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            
            do i = 1, size(proc_node%param_indices)
                if (proc_node%param_indices(i) > 0 .and. &
                    proc_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(proc_node%param_indices(i))%node)) then
                        select type (param_node => arena%entries(proc_node%param_indices(i))%node)
                        type is (identifier_node)
                            if (param_node%name == var_name) then
                                is_param = .true.
                                return
                            end if
                        end select
                    end if
                end if
            end do
            
        type is (subroutine_def_node)
            if (.not. allocated(proc_node%param_indices)) return
            
            do i = 1, size(proc_node%param_indices)
                if (proc_node%param_indices(i) > 0 .and. &
                    proc_node%param_indices(i) <= arena%size) then
                    if (allocated(arena%entries(proc_node%param_indices(i))%node)) then
                        select type (param_node => arena%entries(proc_node%param_indices(i))%node)
                        type is (identifier_node)
                            if (param_node%name == var_name) then
                                is_param = .true.
                                return
                            end if
                        end select
                    end if
                end if
            end do
        end select
    end function is_function_parameter

    ! Check if a name is in parameter names array
    function is_parameter_name(var_name, param_names) result(is_param)
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: param_names(:)
        logical :: is_param
        integer :: i
        
        is_param = .false.
        do i = 1, size(param_names)
            if (trim(param_names(i)) == trim(var_name)) then
                is_param = .true.
                return
            end if
        end do
    end function is_parameter_name

    ! Add line continuations to long lines
    function add_line_continuations(input_code) result(output_code)
        character(len=*), intent(in) :: input_code
        character(len=:), allocatable :: output_code
        integer, parameter :: MAX_LINE_LENGTH = 132
        character(len=:), allocatable :: lines(:)
        integer :: i
        
        ! For now, return input unchanged
        ! Full implementation would split long lines with proper continuations
        output_code = input_code
    end function add_line_continuations

    ! Add a line with continuation if needed
    subroutine add_line_with_continuation(long_line, max_length, output_code)
        character(len=*), intent(in) :: long_line
        integer, intent(in) :: max_length
        character(len=:), allocatable, intent(inout) :: output_code
        integer :: pos, last_break
        character(len=:), allocatable :: current_line
        
        if (len_trim(long_line) <= max_length) then
            output_code = output_code // long_line // new_line('A')
            return
        end if
        
        ! Split at appropriate positions (commas, operators, etc.)
        pos = 1
        do while (pos <= len_trim(long_line))
            last_break = min(pos + max_length - 2, len_trim(long_line))
            
            ! Find a good break point
            ! For now, just break at max_length
            current_line = long_line(pos:last_break)
            
            if (last_break < len_trim(long_line)) then
                output_code = output_code // current_line // " &" // new_line('A')
            else
                output_code = output_code // current_line // new_line('A')
            end if
            
            pos = last_break + 1
        end do
    end subroutine add_line_with_continuation

end module codegen_utilities