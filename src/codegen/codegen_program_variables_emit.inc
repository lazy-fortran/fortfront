
    ! Check if a call name matches a variable type (type constructor)
    logical function is_type_constructor_call(state, call_name) result(is_constructor)
        type(program_decl_state_t), intent(in) :: state
        character(len=*), intent(in) :: call_name
        character(len=64) :: normalized_call, normalized_type
        character(len=64) :: type_name
        integer :: i, open_paren, close_paren

        is_constructor = .false.
        normalized_call = trim(to_lower(call_name))

        ! Check if call_name matches any variables type
        do i = 1, state%var_count
            normalized_type = trim(adjustl(to_lower(state%var_types(i))))

            ! Extract type name from type(typename) format
            if (index(normalized_type, 'type(') == 1) then
                ! Find the type name between type( and )
                open_paren = 5  ! Position after type(
                close_paren = index(normalized_type, ')')
                if (close_paren > open_paren) then
                    type_name = normalized_type(open_paren + 1:close_paren - 1)
                    type_name = trim(adjustl(type_name))

                    ! If call name matches the type name, its a type constructor
                    if (len_trim(type_name) > 0 .and. normalized_call == type_name) then
                        is_constructor = .true.
                        return
                    end if
                end if
            end if
        end do
    end function is_type_constructor_call

    function emit_program_declarations(state) result(code)
        type(program_decl_state_t), intent(in) :: state
        character(len=:), allocatable :: code
        integer :: i, j
        character(len=:), allocatable :: current_type, var_list
        logical :: type_used(program_decl_max_vars)
        logical :: first_var

        code = ""
        type_used = .false.

        do i = 1, state%var_count
            if (type_used(i)) cycle

            current_type = trim(state%var_types(i))
            var_list = trim(state%var_names(i))
            type_used(i) = .true.
            first_var = .true.

            do j = i + 1, state%var_count
                if (type_used(j)) cycle
                if (trim(state%var_types(j)) == current_type) then
                    if (first_var) then
                        first_var = .false.
                    end if
                    var_list = var_list // ", " // trim(state%var_names(j))
                    type_used(j) = .true.
                end if
            end do

            code = code // "    " // current_type // " :: " // var_list // &
                   new_line('A')
        end do

        do i = 1, state%func_count
            if (exists_in_list(state%internal_funcs, state%internal_count, &
                               trim(state%func_names(i)))) cycle
            if (is_intrinsic_function(trim(state%func_names(i)))) cycle
            if (module_is_used(state, "auto_"//trim(state%func_names(i)))) cycle
            if (exists_in_list(state%use_associated_names, &
                               state%use_associated_count, &
                               trim(state%func_names(i)))) cycle
            ! Skip if the name matches a variable type (likely a type constructor)
            if (is_type_constructor_call(state, trim(state%func_names(i)))) cycle
            code = code // "    " // trim(state%func_types(i)) // &
                   ", external :: " // trim(state%func_names(i)) // new_line('A')
        end do
    end function emit_program_declarations
