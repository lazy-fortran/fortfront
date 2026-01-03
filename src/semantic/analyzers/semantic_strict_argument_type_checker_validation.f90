	module semantic_strict_argument_type_checker_validation
	    use ast_arena_modern, only: ast_arena_t
	    use ast_nodes_core, only: assignment_node, identifier_node
	    use ast_nodes_data, only: declaration_node, parameter_declaration_node
	    use error_handling, only: ERROR_SEMANTIC, create_error_result, error_collection_t
	    use semantic_unsigned_integer_mix_diagnostics, only: &
	        emit_unsigned_integer_mix_error, extract_integer_signedness, &
	        is_integer_literal_expr
	    use semantic_strict_argument_type_checker_types, only: &
	        strict_actual_argument_type, &
	        strict_dummy_type, strict_type_is_known, strict_type_name, strict_types_match
	    use string_utils_mod, only: to_lower
	    use type_system_unified, only: mono_type_t
    implicit none
    private

    public :: validate_call_against_interface

contains

    subroutine validate_call_against_interface(arena, errors, proc_name, &
                                               arg_indices, param_indices, body_indices)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: proc_name
        integer, allocatable, intent(in) :: arg_indices(:)
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)

        integer :: i
        integer :: next_positional
        integer :: dummy_count
        integer :: actual_count
        integer, allocatable :: arg_for_param(:)
        integer, allocatable :: decl_for_param(:)
        character(len=:), allocatable :: param_names(:)
        character(len=64) :: keyword
        logical :: is_keyword

        if (.not. allocated(param_indices)) return
        dummy_count = size(param_indices)
        if (dummy_count <= 0) return

        actual_count = 0
        if (allocated(arg_indices)) actual_count = size(arg_indices)

        allocate (arg_for_param(dummy_count))
        arg_for_param = 0
        allocate (character(len=64) :: param_names(dummy_count))
        allocate (decl_for_param(dummy_count))
        decl_for_param = 0

        do i = 1, dummy_count
            param_names(i) = resolve_param_name(arena, param_indices(i), i)
        end do
        call collect_param_decl_indices(arena, body_indices, param_names, &
                                        decl_for_param)

        if (allocated(arg_indices)) then
            do i = 1, actual_count
                call map_keyword_argument(arena, errors, arg_indices(i), param_names, &
                                          arg_for_param)
            end do

            next_positional = 1
            do i = 1, actual_count
                call extract_keyword_name_if_present(arena, arg_indices(i), keyword, &
                                                     is_keyword)
                if (is_keyword) cycle
                call map_positional_argument(errors, arg_indices(i), next_positional, &
                                             arg_for_param)
            end do
        end if

        do i = 1, dummy_count
            if (arg_for_param(i) == 0) cycle
            call validate_mapped_argument(arena, errors, proc_name, &
                                          trim(param_names(i)), arg_for_param(i), &
                                          merge(decl_for_param(i), param_indices(i), &
                                                decl_for_param(i) > 0))
        end do
    end subroutine validate_call_against_interface

    subroutine collect_param_decl_indices(arena, body_indices, param_names, &
                                          decl_for_param)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: param_names(:)
        integer, intent(inout) :: decl_for_param(:)

        integer :: i
        integer :: j
        integer :: stmt_index
        character(len=:), allocatable :: lowered

        if (.not. allocated(body_indices)) return
        if (size(body_indices) == 0) return

        do i = 1, size(body_indices)
            stmt_index = body_indices(i)
            if (.not. arena%has_node_at(stmt_index)) cycle
            select type (node => arena%entries(stmt_index)%node)
            type is (declaration_node)
                if (node%is_multi_declaration .and. allocated(node%var_names)) then
                    do j = 1, size(node%var_names)
                        lowered = to_lower(trim(node%var_names(j)))
                        call bind_decl_index(param_names, lowered, stmt_index, &
                                             decl_for_param)
                    end do
                else if (allocated(node%var_name)) then
                    lowered = to_lower(trim(node%var_name))
                    call bind_decl_index(param_names, lowered, stmt_index, &
                                         decl_for_param)
                end if
            type is (parameter_declaration_node)
                if (.not. allocated(node%name)) cycle
                lowered = to_lower(trim(node%name))
                call bind_decl_index(param_names, lowered, stmt_index, decl_for_param)
            class default
                cycle
            end select
        end do
    end subroutine collect_param_decl_indices

    subroutine bind_decl_index(param_names, lowered_name, decl_index, decl_for_param)
        character(len=*), intent(in) :: param_names(:)
        character(len=*), intent(in) :: lowered_name
        integer, intent(in) :: decl_index
        integer, intent(inout) :: decl_for_param(:)

        integer :: i

        if (len_trim(lowered_name) == 0) return
        do i = 1, size(param_names)
            if (decl_for_param(i) /= 0) cycle
            if (to_lower(trim(param_names(i))) == lowered_name) then
                decl_for_param(i) = decl_index
                return
            end if
        end do
    end subroutine bind_decl_index

    subroutine map_keyword_argument(arena, errors, arg_index, param_names, &
                                    arg_for_param)
        type(ast_arena_t), intent(in) :: arena
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: arg_index
        character(len=*), intent(in) :: param_names(:)
        integer, intent(inout) :: arg_for_param(:)

        integer :: i
        character(len=64) :: keyword
        logical :: is_keyword

        call extract_keyword_name_if_present(arena, arg_index, keyword, is_keyword)
        if (.not. is_keyword) return

        do i = 1, size(param_names)
            if (to_lower(trim(param_names(i))) == to_lower(trim(keyword))) then
                if (arg_for_param(i) /= 0) then
                    call emit_argument_error(errors, &
                                             "Duplicate argument for dummy '"// &
                                             trim(param_names(i))//"'")
                    return
                end if
                arg_for_param(i) = value_index_if_keyword_arg(arena, arg_index)
                return
            end if
        end do

        call emit_argument_error(errors, &
                                 "Unknown keyword argument '"//trim(keyword)//"'")
    end subroutine map_keyword_argument

    subroutine map_positional_argument(errors, arg_index, next_positional, &
                                       arg_for_param)
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: arg_index
        integer, intent(inout) :: next_positional
        integer, intent(inout) :: arg_for_param(:)

        do while (next_positional <= size(arg_for_param))
            if (arg_for_param(next_positional) == 0) exit
            next_positional = next_positional + 1
        end do

        if (next_positional > size(arg_for_param)) then
            call emit_argument_error(errors, "Too many actual arguments")
            return
        end if

        arg_for_param(next_positional) = arg_index
        next_positional = next_positional + 1
    end subroutine map_positional_argument

	    subroutine validate_mapped_argument(arena, errors, proc_name, dummy_name, &
	                                        arg_expr_index, dummy_index)
        type(ast_arena_t), intent(inout) :: arena
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: proc_name
        character(len=*), intent(in) :: dummy_name
        integer, intent(in) :: arg_expr_index
        integer, intent(in) :: dummy_index

	        type(mono_type_t) :: actual_type
	        type(mono_type_t) :: dummy_type
	        character(len=:), allocatable :: actual_string
	        character(len=:), allocatable :: dummy_string
	        logical :: actual_is_int
	        logical :: dummy_is_int
	        logical :: actual_is_unsigned
	        logical :: dummy_is_unsigned
	        logical :: actual_is_int_literal

        if (.not. arena%has_node_at(arg_expr_index)) return
        if (.not. arena%has_node_at(dummy_index)) return

        actual_type = strict_actual_argument_type(arena, arg_expr_index)
        dummy_type = strict_dummy_type(arena, dummy_index)
        call actual_type%sync_from_arena()
        call dummy_type%sync_from_arena()

	        if (.not. strict_type_is_known(actual_type)) return
	        if (.not. strict_type_is_known(dummy_type)) return

	        call extract_integer_signedness(actual_type, actual_is_int, &
	                                       actual_is_unsigned)
	        call extract_integer_signedness(dummy_type, dummy_is_int, &
	                                       dummy_is_unsigned)
	        actual_is_int_literal = is_integer_literal_expr(arena, arg_expr_index)

	        if (actual_is_int .and. dummy_is_int) then
	            if (actual_is_unsigned .neqv. dummy_is_unsigned) then
	                if (.not. actual_is_int_literal) then
	                    call emit_unsigned_integer_mix_error(errors)
	                    return
	                end if
	            end if
	        end if

	        if (strict_types_match(dummy_type, actual_type)) return

        call strict_type_name(actual_type, actual_string)
        call strict_type_name(dummy_type, dummy_string)
        call emit_argument_error(errors, &
                                 "Type mismatch in call to '"//trim(proc_name)// &
                                 "': actual argument '"//trim(dummy_name)// &
                                 "' is "//trim(actual_string)// &
                                 ", but dummy expects "//trim(dummy_string))
    end subroutine validate_mapped_argument

    subroutine extract_keyword_name_if_present(arena, arg_index, keyword, is_keyword)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arg_index
        character(len=64), intent(out) :: keyword
        logical, intent(out) :: is_keyword

        keyword = ""
        is_keyword = .false.
        if (.not. arena%has_node_at(arg_index)) return

        select type (node => arena%entries(arg_index)%node)
        type is (assignment_node)
            if (.not. node%is_keyword_argument) return
            keyword = keyword_target_name(arena, node%target_index)
            if (len_trim(keyword) == 0) return
            is_keyword = .true.
        class default
            return
        end select
    end subroutine extract_keyword_name_if_present

    integer function value_index_if_keyword_arg(arena, arg_index) result(value_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: arg_index

        value_index = arg_index
        if (.not. arena%has_node_at(arg_index)) return

        select type (node => arena%entries(arg_index)%node)
        type is (assignment_node)
            if (.not. node%is_keyword_argument) return
            value_index = node%value_index
        class default
            value_index = arg_index
        end select
    end function value_index_if_keyword_arg

    character(len=64) function keyword_target_name(arena, target_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: target_index

        name = ""
        if (.not. arena%has_node_at(target_index)) return

        select type (node => arena%entries(target_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
        type is (declaration_node)
            if (allocated(node%var_name)) name = trim(node%var_name)
        type is (parameter_declaration_node)
            if (allocated(node%name)) name = trim(node%name)
        class default
            return
        end select
    end function keyword_target_name

    character(len=64) function resolve_param_name(arena, param_index, position) &
        result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        integer, intent(in) :: position

        name = "arg"
        if (param_index <= 0 .or. param_index > arena%size) then
            write (name, '(A,I0)') "arg", position
            return
        end if
        if (.not. allocated(arena%entries(param_index)%node)) then
            write (name, '(A,I0)') "arg", position
            return
        end if

        select type (node => arena%entries(param_index)%node)
        type is (identifier_node)
            if (allocated(node%name)) then
                if (len_trim(node%name) > 0) then
                    name = trim(node%name)
                    return
                end if
            end if
        type is (parameter_declaration_node)
            if (allocated(node%name)) then
                if (len_trim(node%name) > 0) then
                    name = trim(node%name)
                    return
                end if
            end if
        type is (declaration_node)
            if (allocated(node%var_name)) then
                if (len_trim(node%var_name) > 0) then
                    name = trim(node%var_name)
                    return
                end if
            end if
        class default
            continue
        end select

        write (name, '(A,I0)') "arg", position
    end function resolve_param_name

    subroutine emit_argument_error(errors, message)
        type(error_collection_t), intent(inout) :: errors
        character(len=*), intent(in) :: message

        call errors%add_result(create_error_result( &
                               trim(message), ERROR_SEMANTIC, &
                               component="semantic_analyzer", &
                               context="strict_argument_type_check", &
                               suggestion="Use explicit conversion functions "// &
                               "(real/int/dble) to match dummy "// &
                               "argument types"))
    end subroutine emit_argument_error

end module semantic_strict_argument_type_checker_validation
