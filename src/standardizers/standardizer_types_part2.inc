
    function build_nested_array_type(node, elem_type_str, all_literal_arrays, &
                                      inner_size) result(var_type)
        type(array_literal_node), intent(in) :: node
        character(len=*), intent(in) :: elem_type_str
        logical, intent(in) :: all_literal_arrays
        integer, intent(in) :: inner_size
        character(len=64) :: var_type

        if (all_literal_arrays .and. inner_size > 0) then
            write (var_type, '(a,a,i0,a,i0,a)') trim(elem_type_str), &
                ", dimension(", size(node%element_indices), ",", inner_size, ")"
        else
            var_type = trim(elem_type_str) // ", dimension(:), allocatable"
        end if
    end function build_nested_array_type

    ! Helper function to infer type from a literal node
    function infer_element_type_from_literal(arena, elem_index) result(type_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: elem_index
        character(len=:), allocatable :: type_str
        logical :: standardizer_type_standardization_enabled

        type_str = ""  ! Empty default indicates type not determined

     call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        if (.not. arena%has_node_at(elem_index)) return

        select type (elem => arena%entries(elem_index)%node)
        type is (literal_node)
            select case (elem%literal_kind)
            case (LITERAL_INTEGER)
                type_str = "integer"
            case (LITERAL_REAL)
                if (standardizer_type_standardization_enabled) then
                    type_str = "real(dp)"
                else
                    type_str = "real"
                end if
            case (LITERAL_STRING)
                type_str = "character"
            case (LITERAL_LOGICAL)
                type_str = "logical"
            case default
                type_str = ""  ! No fallback for unknown literal types
            end select
        end select
    end function infer_element_type_from_literal

    ! Convert mono_type_t to Fortran type string
    recursive function get_fortran_type_string(mono_type) result(string_result)
        type(mono_type_t), intent(in) :: mono_type
        type(string_result_t) :: string_result
        logical :: standardizer_type_standardization_enabled
        logical :: type_success
        character(len=:), allocatable :: type_string
        character(len=:), allocatable :: element_type

        call get_standardizer_type_standardization( &
            standardizer_type_standardization_enabled)

        if (mono_type%kind == TARRAY) then
            if (type_args_allocated(mono_type) .and. &
                type_args_size(mono_type) > 0) then
                element_type = mono_type_to_string( &
                               type_args_element(mono_type, 1), include_shape=.false., &
                               prefer_len_zero_char=.true., &
                             standardize_real=standardizer_type_standardization_enabled, &
                               success=type_success)
                if (type_success) then
                    string_result%result = success_result()
                    string_result%value = element_type
                else
                    string_result%result = create_error_result( &
                                           "Failed to determine array element type", &
                                           ERROR_TYPE_SYSTEM, &
                                           component="standardizer", &
                                           context="get_fortran_type_string", &
                             suggestion="Ensure array element type is properly inferred" &
                                           )
                end if
            else
                string_result%result = create_error_result( &
                                       "Array type has no element type information", &
                                       ERROR_TYPE_SYSTEM, &
                                       component="standardizer", &
                                       context="get_fortran_type_string", &
         suggestion="Array type should have at least one type argument for element type" &
                                       )
            end if
            return
        end if

        type_string = mono_type_to_string(mono_type, include_shape=.false., &
                                          prefer_len_zero_char=.true., &
                             standardize_real=standardizer_type_standardization_enabled, &
                                          success=type_success)

        if (type_success) then
            string_result%result = success_result()
            string_result%value = type_string
        else
            string_result%result = create_error_result( &
                                   "Unknown or unsupported type kind", &
                                   ERROR_TYPE_SYSTEM, &
                                   component="standardizer", &
                                   context="get_fortran_type_string", &
          suggestion="Type inference may have failed or encountered an unsupported type" &
                                   )
        end if
    end function get_fortran_type_string

    ! String result methods
    function string_is_success(this) result(success)
        class(string_result_t), intent(in) :: this
        logical :: success
        success = this%result%success
    end function string_is_success

    function string_get_value(this) result(value)
        class(string_result_t), intent(in) :: this
        character(len=:), allocatable :: value
        if (this%result%success .and. allocated(this%value)) then
            value = this%value
        else
            value = ""
        end if
    end function string_get_value

    function string_get_error(this) result(error_msg)
        class(string_result_t), intent(in) :: this
        character(len=:), allocatable :: error_msg
        error_msg = this%result%error_message
    end function string_get_error
