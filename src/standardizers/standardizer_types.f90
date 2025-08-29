module standardizer_types
    ! Type inference and utilities module
    ! Handles type analysis, expression type detection, and type string generation
    
    use ast_core
    use type_system_unified
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use error_handling, only: result_t, success_result, create_error_result, ERROR_TYPE_SYSTEM
    implicit none
    private

    ! Constants
    integer, parameter :: INVALID_INTEGER = -999999

    ! Type standardization configuration (local copy)
    logical, save :: standardizer_type_standardization_enabled = .true.

    ! Result type for string operations
    type, public :: string_result_t
        type(result_t) :: result
        character(len=:), allocatable :: value  ! Valid only if result%success = .true.
    contains
        procedure :: is_success => string_is_success
        procedure :: get_value => string_get_value
        procedure :: get_error => string_get_error
    end type string_result_t

    public :: is_array_type
    public :: get_expression_type
    public :: has_array_slice_args
    public :: is_array_expression
    public :: has_implied_do_loop
    public :: get_implied_do_size
    public :: calculate_loop_size
    public :: get_integer_literal_value
    public :: get_array_var_type
    public :: infer_element_type_from_literal
    public :: get_fortran_type_string
    public :: INVALID_INTEGER

contains

    ! Local implementation of get_standardizer_type_standardization
    subroutine get_standardizer_type_standardization(enabled)
        logical, intent(out) :: enabled
        enabled = standardizer_type_standardization_enabled
    end subroutine get_standardizer_type_standardization

    ! Check if a mono_type is an array type
    function is_array_type(mono_type) result(is_array)
        type(mono_type_t), intent(in) :: mono_type
        logical :: is_array
        
        is_array = (mono_type%kind == TARRAY)
    end function is_array_type
    
    ! Get the type of an expression from the AST
    function get_expression_type(arena, expr_index) result(expr_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t), pointer :: expr_type
        
        expr_type => null()
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            ! Handle literal nodes by checking their inferred type or literal kind
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            else
                ! Fallback to determining type from literal_kind
                allocate(expr_type)
                select case (node%literal_kind)
                case (LITERAL_INTEGER)
                    expr_type = create_mono_type(TINT)
                case (LITERAL_REAL)
                    expr_type = create_mono_type(TREAL)
                case (LITERAL_STRING)
                    expr_type = create_mono_type(TCHAR)
                    ! Set the size based on the string literal length (excluding quotes)
                    if (allocated(node%value)) then
                        if (len(node%value) >= 2) then
                            expr_type%size = len(node%value) - 2  ! Remove surrounding quotes
                        else
                            expr_type%size = 0  ! Empty string
                        end if
                    end if
                case (LITERAL_LOGICAL)
                    expr_type = create_mono_type(TLOGICAL)
                case default
                    expr_type = create_mono_type(TREAL)  ! Default fallback
                end select
            end if
        type is (identifier_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        type is (array_literal_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        type is (call_or_subscript_node)
            ! Check if this is an array subscript
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            else
                ! If it's a subscript of an array, the result should be the 
                ! element type or a subarray
                ! For now, we'll check if it has a colon operator (array slice)
                if (has_array_slice_args(arena, node)) then
                    ! This is an array slice, so result is an array
                    ! We need to get the base array type
                    allocate(expr_type)
                    expr_type%kind = TARRAY
                    ! TODO: Set proper element type
                end if
            end if
        type is (binary_op_node)
            if (node%inferred_type%kind > 0) then
                expr_type => node%inferred_type
            end if
        end select
    end function get_expression_type
    
    ! Check if a call_or_subscript node has array slice arguments
    function has_array_slice_args(arena, node) result(has_slice)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: node
        logical :: has_slice
        integer :: i
        
        has_slice = .false.
        
        if (.not. allocated(node%arg_indices)) return
        
        do i = 1, size(node%arg_indices)
            if (node%arg_indices(i) > 0 .and. node%arg_indices(i) <= arena%size) then
                if (allocated(arena%entries(node%arg_indices(i))%node)) then
                    select type (arg => arena%entries(node%arg_indices(i))%node)
                    type is (binary_op_node)
                        if (trim(arg%operator) == ":") then
                            has_slice = .true.
                            return
                        end if
                    end select
                end if
            end if
        end do
    end function has_array_slice_args
    
    ! Check if an expression is an array expression by structure
    function is_array_expression(arena, expr_index) result(is_array)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        logical :: is_array
        
        is_array = .false.
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            is_array = .true.
        type is (call_or_subscript_node)
            ! Check if this is an array slice (has colon operator in args)
            if (has_array_slice_args(arena, node)) then
                is_array = .true.
            end if
        end select
    end function is_array_expression
    
    ! Check if array literal contains an implied do loop
    function has_implied_do_loop(arena, array_node) result(has_implied)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: array_node
        logical :: has_implied
        
        has_implied = .false.
        
        if (allocated(array_node%element_indices)) then
            if (size(array_node%element_indices) == 1) then
                ! Check if the single element is a do_loop_node
                if (array_node%element_indices(1) > 0 .and. &
                    array_node%element_indices(1) <= arena%size) then
                    if (allocated(arena%entries(array_node%element_indices(1))%node)) then
                        select type (elem => arena%entries(array_node%element_indices(1))%node)
                        type is (do_loop_node)
                            has_implied = .true.
                        class default
                        end select
                    end if
                end if
            end if
        end if
    end function has_implied_do_loop
    
    ! Calculate size of implied do loop
    function get_implied_do_size(arena, do_node_index) result(size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: do_node_index
        integer :: size
        
        size = -1  ! Return -1 if we can't determine the size
        
        if (do_node_index <= 0 .or. do_node_index > arena%size) return
        if (.not. allocated(arena%entries(do_node_index)%node)) return
        
        select type (do_node => arena%entries(do_node_index)%node)
        type is (do_loop_node)
            ! Try to calculate size from start, end, and step expressions
            ! For now, we'll handle simple integer literals
            if (do_node%start_expr_index > 0 .and. do_node%end_expr_index > 0) then
                size = calculate_loop_size(arena, do_node%start_expr_index, &
                                          do_node%end_expr_index, do_node%step_expr_index)
            end if
        end select
    end function get_implied_do_size
    
    ! Calculate loop size from start, end, and step expressions
    function calculate_loop_size(arena, start_idx, end_idx, step_idx) result(size)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: start_idx, end_idx, step_idx
        integer :: size
        integer :: start_val, end_val, step_val
        
        size = -1
        
        ! Get start value
        start_val = get_integer_literal_value(arena, start_idx)
        if (start_val == INVALID_INTEGER) then
            return
        end if
        
        ! Get end value
        end_val = get_integer_literal_value(arena, end_idx)
        if (end_val == INVALID_INTEGER) then
            return
        end if
        
        ! Get step value (default to 1 if not specified)
        if (step_idx > 0) then
            step_val = get_integer_literal_value(arena, step_idx)
            if (step_val == INVALID_INTEGER) step_val = 1
        else
            step_val = 1
        end if
        
        ! Calculate size
        if (step_val /= 0) then
            if (step_val > 0) then
                ! Forward iteration
                if (end_val >= start_val) then
                    size = (end_val - start_val) / step_val + 1
                else
                    size = 0  ! No iterations
                end if
            else
                ! Backward iteration
                if (start_val >= end_val) then
                    size = (start_val - end_val) / abs(step_val) + 1
                else
                    size = 0  ! No iterations
                end if
            end if
        end if
    end function calculate_loop_size
    
    ! Get integer value from a literal node
    recursive function get_integer_literal_value(arena, expr_idx) result(value)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_idx
        integer :: value
        
        value = INVALID_INTEGER  ! Error value
        
        if (expr_idx <= 0 .or. expr_idx > arena%size) then
            return
        end if
        if (.not. allocated(arena%entries(expr_idx)%node)) then
            return
        end if
        
        select type (node => arena%entries(expr_idx)%node)
        type is (literal_node)
            ! Try using literal_kind instead of literal_type
            if (node%literal_kind == LITERAL_INTEGER .and. allocated(node%value)) then
                block
                    integer :: iostat
                    read(node%value, *, iostat=iostat) value
                    if (iostat /= 0) then
                        value = INVALID_INTEGER
                    end if
                end block
            end if
        type is (binary_op_node)
            ! Handle simple binary operations for compile-time constants
            if (allocated(node%operator)) then
                if (node%left_index > 0 .and. node%right_index > 0) then
                    block
                        integer :: left_val, right_val
                        left_val = get_integer_literal_value(arena, node%left_index)
                        right_val = get_integer_literal_value(arena, node%right_index)
                        if (left_val /= INVALID_INTEGER .and. &
                            right_val /= INVALID_INTEGER) then
                            select case(node%operator)
                            case("-")
                                value = left_val - right_val
                            case("+")
                                value = left_val + right_val
                            case("*")
                                value = left_val * right_val
                            case("/")
                                if (right_val /= 0) value = left_val / right_val
                            end select
                        end if
                    end block
                end if
            end if
        class default
        end select
    end function get_integer_literal_value
    
    ! Get array variable type declaration from an array expression
    function get_array_var_type(arena, expr_index) result(var_type)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        character(len=64) :: var_type
        type(mono_type_t), pointer :: expr_type
        character(len=:), allocatable :: elem_type_str
        
        var_type = ""  ! Empty default indicates type not determined
        
        if (expr_index <= 0 .or. expr_index > arena%size) return
        if (.not. allocated(arena%entries(expr_index)%node)) return
        
        select type (node => arena%entries(expr_index)%node)
        type is (array_literal_node)
            ! For array literals, we know the exact size
            if (allocated(node%element_indices)) then
                ! Try to get the inferred type of the array literal
                if (node%inferred_type%kind > 0) then
                    ! The inferred type is TARRAY with element type in args(1)
                    ! get_fortran_type_string handles TARRAY by extracting element type
                    block
                        type(string_result_t) :: type_result
                        type_result = get_fortran_type_string(node%inferred_type)
                        if (type_result%is_success()) then
                            elem_type_str = type_result%get_value()
                        else
                            elem_type_str = ""
                        end if
                    end block
                else
                    ! No inferred type, try to determine from elements
                    if (size(node%element_indices) > 0) then
                        ! Check the first element type
                        expr_type => get_expression_type(arena, node%element_indices(1))
                        if (associated(expr_type)) then
                            block
                                type(string_result_t) :: type_result
                                type_result = get_fortran_type_string(expr_type)
                                if (type_result%is_success()) then
                                    elem_type_str = type_result%get_value()
                                else
                                    elem_type_str = ""
                                end if
                            end block
                        else
                            ! Try to infer from literal if possible
                            elem_type_str = &
                                infer_element_type_from_literal(arena, &
                                node%element_indices(1))
                        end if
                    else
                        elem_type_str = ""  ! No fallback for empty arrays
                    end if
                end if
                
                ! Check if this is an implied do loop
                if (has_implied_do_loop(arena, node)) then
                    ! Calculate the size from the implied do loop bounds
                    block
                        integer :: implied_size
                        implied_size = get_implied_do_size(arena, node%element_indices(1))
                        if (implied_size > 0) then
                            write(var_type, '(a,a,i0,a)') trim(elem_type_str), &
                                ", dimension(", implied_size, ")"
                        else
                            ! Can't determine size, use allocatable
                            var_type = trim(elem_type_str) // ", dimension(:), allocatable"
                        end if
                    end block
                else
                    ! Regular array literal with explicit elements
                    write(var_type, '(a,a,i0,a)') trim(elem_type_str), &
                        ", dimension(", size(node%element_indices), ")"
                end if
            end if
        type is (call_or_subscript_node)
            ! For array slices, type cannot be determined without more context
            if (has_array_slice_args(arena, node)) then
                ! Type cannot be determined for array slices without element type
                var_type = ""
            end if
        end select
    end function get_array_var_type
    
    ! Helper function to infer type from a literal node
    function infer_element_type_from_literal(arena, elem_index) result(type_str)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: elem_index
        character(len=:), allocatable :: type_str
        logical :: standardizer_type_standardization_enabled
        
        type_str = ""  ! Empty default indicates type not determined
        
        call get_standardizer_type_standardization(standardizer_type_standardization_enabled)
        
        if (elem_index <= 0 .or. elem_index > arena%size) return
        if (.not. allocated(arena%entries(elem_index)%node)) return
        
        select type (elem => arena%entries(elem_index)%node)
        type is (literal_node)
            select case (elem%literal_kind)
            case (LITERAL_INTEGER)
                type_str = "integer"
            case (LITERAL_REAL)
                if (standardizer_type_standardization_enabled) then
                    type_str = "real(8)"
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
        type(string_result_t) :: elem_result
        logical :: standardizer_type_standardization_enabled
        
        call get_standardizer_type_standardization(standardizer_type_standardization_enabled)

        select case (mono_type%kind)
        case (TINT)
            string_result%result = success_result()
            string_result%value = "integer"
        case (TREAL)
            string_result%result = success_result()
            if (standardizer_type_standardization_enabled) then
                string_result%value = "real(8)"
            else
                string_result%value = "real"
            end if
        case (TLOGICAL)
            string_result%result = success_result()
            string_result%value = "logical"
        case (TCHAR)
            string_result%result = success_result()
            if (mono_type%alloc_info%needs_allocatable_string) then
                string_result%value = "character(len=:), allocatable"
            else if (mono_type%size > 0) then
                block
                    character(len=20) :: size_str
                    write (size_str, '(i0)') mono_type%size
                    string_result%value = "character(len="//trim(size_str)//")"
                end block
            else
                ! For zero-length or unknown strings, use explicit length 0
                ! character(*) is only valid in parameter declarations, not variable declarations
                string_result%value = "character(len=0)"
            end if
        case (TARRAY)
            ! For arrays, get the element type
            if (type_args_allocated(mono_type) .and. type_args_size(mono_type) > 0) then
                elem_result = get_fortran_type_string(type_args_element(mono_type, 1))
                if (elem_result%is_success()) then
                    string_result%result = success_result()
                    string_result%value = elem_result%get_value()
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
        case default
            string_result%result = create_error_result( &
                "Unknown or unsupported type kind", &
                ERROR_TYPE_SYSTEM, &
                component="standardizer", &
                context="get_fortran_type_string", &
                suggestion="Type inference may have failed or encountered an unsupported type" &
            )
        end select
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

end module standardizer_types