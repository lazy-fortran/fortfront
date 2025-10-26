module standardizer_declarations_collection
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, binary_op_node, &
                              call_or_subscript_node, identifier_node, literal_node
    use ast_nodes_data, only: declaration_node
    use ast_nodes_loops, only: do_loop_node, do_while_node
    use ast_nodes_control, only: if_node, select_case_node, case_block_node, &
                                 case_default_node
    use ast_nodes_io, only: io_implied_do_node, print_statement_node, &
                            read_statement_node
    use ast_base, only: LITERAL_INTEGER, LITERAL_LOGICAL, LITERAL_STRING
    use standardizer_declarations_state, only: get_standardizer_type_standardization
    use standardizer_declarations_inference, only: &
        handle_string_concatenation, &
        infer_type_from_binary_operation, &
        get_string_length_from_node, &
        infer_type_from_intrinsic_call, &
        build_character_type_from_length, &
        merge_character_type_lengths, &
        is_integer_expression
    use standardizer_declarations_variables, only: &
        add_variable, mark_variable_declared, &
        collect_identifier_var_with_type, collect_identifier_var
    use standardizer_types, only: get_array_var_type, get_expression_type, &
                                  get_fortran_type_string, is_array_expression, &
                                  string_result_t
    use string_utils_mod, only: int_to_string
    use lexer_core, only: to_lower
    use type_string_utils, only: is_character_type_string
    use type_system_unified, only: mono_type_t
    implicit none
    private

    public :: collect_statement_vars
    public :: collect_assignment_vars
    public :: handle_string_concatenation
    public :: infer_type_from_binary_operation
    public :: get_string_length_from_node

contains

    subroutine collect_statement_vars(arena, stmt_index, var_names, &
                                      var_types, var_declared, var_count, &
                                      function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: stmt_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count

        type stack_entry
            integer :: idx = 0
        end type stack_entry

        type(stack_entry), allocatable :: stack(:)
        integer :: capacity, top
        integer :: current_index
        integer :: j

        capacity = 128
        allocate (stack(capacity))
        top = 0

        call push(stmt_index)

        do while (top > 0)
            current_index = pop()
            if (current_index <= 0 .or. current_index > arena%size) cycle
            if (.not. allocated(arena%entries(current_index)%node)) cycle

            select type (stmt => arena%entries(current_index)%node)
            type is (declaration_node)
                if (stmt%is_multi_declaration .and. allocated(stmt%var_names)) then
                    do j = 1, size(stmt%var_names)
                        call register_decl_var(trim(stmt%var_names(j)), stmt)
                    end do
                else
                    call register_decl_var(trim(stmt%var_name), stmt)
                end if
            type is (assignment_node)
                call collect_assignment_vars(arena, current_index, var_names, &
                                             var_types, var_declared, var_count, &
                                             function_names, func_count)
            type is (do_loop_node)
                call add_variable(stmt%var_name, "integer", var_names, var_types, &
                                  var_declared, var_count, function_names, func_count)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (do_while_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (io_implied_do_node)
                call add_variable(stmt%var_name, "integer", var_names, var_types, &
                                  var_declared, var_count, function_names, func_count)
                if (stmt%expr_index > 0) call push(stmt%expr_index)
            type is (if_node)
                if (allocated(stmt%else_body_indices)) call &
                    push_many(stmt%else_body_indices)
                if (allocated(stmt%then_body_indices)) call &
                    push_many(stmt%then_body_indices)
            type is (select_case_node)
                if (stmt%selector_index > 0) call push(stmt%selector_index)
                if (allocated(stmt%case_indices)) call push_many(stmt%case_indices)
                if (stmt%default_index > 0) call push(stmt%default_index)
            type is (case_block_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (case_default_node)
                if (allocated(stmt%body_indices)) call push_many(stmt%body_indices)
            type is (print_statement_node)
                if (allocated(stmt%expression_indices)) then
                    call push_many(stmt%expression_indices)
                end if
            type is (read_statement_node)
                if (allocated(stmt%var_indices)) then
                    call push_many(stmt%var_indices)
                end if
            type is (identifier_node)
                call collect_identifier_var(stmt, var_names, var_types, &
                                            var_declared, var_count, &
                                            function_names, func_count)
            class default
            end select
        end do

    contains

        subroutine register_decl_var(name, decl)
            character(len=*), intent(in) :: name
            type(declaration_node), intent(in) :: decl
            character(len=:), allocatable :: type_str
            integer :: idx, k

            if (len_trim(name) == 0) return
            type_str = declaration_type_string(decl)
            call add_variable(name, type_str, var_names, var_types, var_declared, &
                              var_count, function_names, func_count)
            call mark_variable_declared(name, var_names, var_declared, var_count)

            idx = 0
            do k = 1, var_count
                if (trim(var_names(k)) == trim(name)) then
                    idx = k
                    exit
                end if
            end do
            if (idx > 0 .and. len_trim(type_str) > 0) then
                var_types(idx) = type_str
            end if
        end subroutine register_decl_var

        function declaration_type_string(decl) result(type_str)
            type(declaration_node), intent(in) :: decl
            character(len=:), allocatable :: type_str
            character(len=32) :: buffer
            integer :: dim_idx, i

            type_str = trim(decl%type_name)
            if (decl%has_kind) then
                buffer = int_to_string(decl%kind_value)
                if (len_trim(buffer) > 0) then
                    type_str = trim(type_str) // "(" // trim(buffer) // ")"
                end if
            end if

            if (decl%is_array .and. allocated(decl%dimension_indices)) then
                type_str = trim(type_str) // ", dimension("
                do i = 1, size(decl%dimension_indices)
                    if (i > 1) type_str = type_str // ","
                    dim_idx = decl%dimension_indices(i)
                    if (dim_idx == 0) then
                        type_str = type_str // ":"
                    else if (dim_idx > 0 .and. dim_idx <= arena%size) then
                        if (allocated(arena%entries(dim_idx)%node)) then
                            select type (dim_node => arena%entries(dim_idx)%node)
                            type is (literal_node)
                                type_str = type_str // trim(dim_node%value)
                            class default
                                type_str = type_str // ":"
                            end select
                        else
                            type_str = type_str // ":"
                        end if
                    else if (dim_idx > arena%size) then
                        buffer = int_to_string(dim_idx)
                        type_str = type_str // trim(buffer)
                    else
                        type_str = type_str // ":"
                    end if
                end do
                type_str = type_str // ")"
            end if

            if (decl%is_allocatable) then
                if (.not. has_attribute(type_str, "allocatable")) then
                    type_str = trim(type_str) // ", allocatable"
                end if
            end if

            if (decl%is_pointer) then
                if (.not. has_attribute(type_str, "pointer")) then
                    type_str = trim(type_str) // ", pointer"
                end if
            end if

            if (decl%is_target) then
                if (.not. has_attribute(type_str, "target")) then
                    type_str = trim(type_str) // ", target"
                end if
            end if

            if (decl%is_parameter) then
                if (.not. has_attribute(type_str, "parameter")) then
                    type_str = trim(type_str) // ", parameter"
                end if
            end if

            if (decl%has_intent .and. allocated(decl%intent)) then
                if (.not. has_attribute(type_str, "intent(")) then
                    type_str = trim(type_str) // ", intent(" // &
                        trim(decl%intent) // ")"
                end if
            end if
        end function declaration_type_string

        pure logical function has_attribute(text, attr) result(found)
            character(len=*), intent(in) :: text
            character(len=*), intent(in) :: attr
            character(len=:), allocatable :: lowered
            integer :: i, char_code

            lowered = trim(text)
            do i = 1, len(lowered)
                char_code = iachar(lowered(i:i))
                if (char_code >= iachar('A') .and. char_code <= iachar('Z')) then
                    lowered(i:i) = achar(char_code + 32)
                end if
            end do
            found = index(lowered, trim(attr)) > 0
        end function has_attribute

        subroutine push(idx)
            integer, intent(in) :: idx
            type(stack_entry), allocatable :: tmp(:)
            if (idx <= 0) return
            if (top >= capacity) then
                allocate (tmp(capacity * 2))
                if (capacity > 0) tmp(1:capacity) = stack(1:capacity)
                call move_alloc(tmp, stack)
                capacity = size(stack)
            end if
            top = top + 1
            stack(top)%idx = idx
        end subroutine push

        subroutine push_many(indices)
            integer, intent(in) :: indices(:)
            integer :: k
            do k = size(indices), 1, -1
                call push(indices(k))
            end do
        end subroutine push_many

        integer function pop()
            if (top <= 0) then
                pop = 0
            else
                pop = stack(top)%idx
                top = top - 1
            end if
        end function pop

    end subroutine collect_statement_vars

    subroutine collect_assignment_vars(arena, assign_index, var_names, &
                                       var_types, var_declared, var_count, &
                                       function_names, func_count)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: assign_index
        character(len=64), intent(inout) :: var_names(:)
        character(len=64), intent(inout) :: var_types(:)
        logical, intent(inout) :: var_declared(:)
        integer, intent(inout) :: var_count
        character(len=64), intent(in) :: function_names(:)
        integer, intent(in) :: func_count
        type(mono_type_t), pointer :: value_type
        character(len=64) :: var_type
        integer :: existing_idx
        integer :: i
        integer :: literal_length

        if (assign_index <= 0 .or. assign_index > arena%size) return
        if (.not. allocated(arena%entries(assign_index)%node)) return

        select type (assign => arena%entries(assign_index)%node)
        type is (assignment_node)
            if (assign%target_index > 0 .and. assign%target_index <= arena%size) then
                if (allocated(arena%entries(assign%target_index)%node)) then
                    select type (target => arena%entries(assign%target_index)%node)
                    type is (identifier_node)
                        var_type = ""
                        existing_idx = 0
                        do i = 1, var_count
                            if (trim(var_names(i)) == trim(target%name)) then
                                existing_idx = i
                                exit
                            end if
                        end do

                        if (assign%value_index > 0 .and. &
                            assign%value_index <= arena%size) then
                            if (allocated(arena%entries(assign%value_index)%node)) then
                                if (is_array_expression(arena, &
                                                        assign%value_index)) then
                                    var_type = get_array_var_type(arena, &
                                                                  assign%value_index)
                                else
                                    value_type => get_expression_type( &
                                                  arena, assign%value_index)
                                    if (associated(value_type)) then
                                        block
                                            type(string_result_t) :: type_result
                                            type_result = &
                                                get_fortran_type_string(value_type)
                                            if (type_result%is_success()) then
                                                var_type = type_result%get_value()
                                            end if
                                        end block
                                    end if

                                    if (len_trim(var_type) == 0) then
                                        call infer_type_from_intrinsic_call( &
                                            arena, assign%value_index, var_type)
                                    end if

                                    if (len_trim(var_type) == 0) then
                                        if (is_integer_expression( &
                                            arena, assign%value_index)) then
                                            var_type = "integer"
                                        end if
                                    end if

                                    if (len_trim(var_type) == 0) then
                                        var_type = handle_string_concatenation( &
                                                   arena, assign%value_index)
                                    end if

                                    if (len_trim(var_type) == 0) then
                                        var_type = infer_type_from_binary_operation( &
                                                   arena, assign%value_index)
                                    end if
                                end if
                            end if
                        end if

                        if (len_trim(var_type) == 0) then
                            literal_length = get_string_length_from_node( &
                                             arena, assign%value_index)
                            if (literal_length >= 0) then
                                var_type = build_character_type_from_length( &
                                           literal_length)
                            end if
                        end if

                        if (len_trim(var_type) == 0) then
                            var_type = "real"
                        end if

                        if (existing_idx > 0) then
                            if (len_trim(var_type) > 0) then
                                if (is_character_type_string(var_types(existing_idx)) &
                                    .and. is_character_type_string(var_type)) then
                                    var_types(existing_idx) = &
                                        merge_character_type_lengths( &
                                        var_types(existing_idx), var_type)
                                else
                                    var_types(existing_idx) = trim(var_type)
                                end if
                            end if
                            if (index(var_types(existing_idx), 'character(') == 1 &
                                .and. index(var_types(existing_idx), 'len=:') > 0 &
                                .and. index(var_types(existing_idx), 'allocatable') &
                                == 0) then
                                var_types(existing_idx) = &
                                    trim(var_types(existing_idx)) &
                                    // ", allocatable"
                            end if
                        else
                            call collect_identifier_var_with_type(target, var_type, &
                                                                  var_names, &
                                                                  var_types, &
                                                                  var_declared, &
                                                                  var_count, &
                                                                  function_names, &
                                                                  func_count)
                        end if
                    type is (call_or_subscript_node)
                        if (target%is_array_access .and. allocated(target%name)) then
                            block
                                character(len=64) :: base_name
                                character(len=96) :: decl_type
                                integer :: rank, idx

                                base_name = trim(target%name)
                                decl_type = ''

                                if (assign%type_was_inferred .and. &
                                    allocated(assign%inferred_type_name)) then
                                    decl_type = trim(assign%inferred_type_name)
                                end if

                                if (len_trim(decl_type) == 0) then
                                    rank = 0
                                    if (allocated(target%arg_indices)) rank = &
                                        size(target%arg_indices)
                                    if (rank <= 0) rank = 1
                                    decl_type = 'real, dimension('
                                    do idx = 1, rank
                                        if (idx > 1) decl_type = trim(decl_type) // ','
                                        decl_type = trim(decl_type) // ':'
                                    end do
                                    decl_type = trim(decl_type) // ')'
                                end if

                                call add_variable(base_name, decl_type, var_names, &
                                                  var_types, &
                                                  var_declared, var_count, &
                                                  function_names, func_count)
                            end block
                        end if
                    end select
                end if
            end if
        end select
    end subroutine collect_assignment_vars

end module standardizer_declarations_collection
