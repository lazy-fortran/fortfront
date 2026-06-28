module codegen_type_inference_utils
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, call_or_subscript_node, &
        array_literal_node
    use string_utils_mod, only: to_lower
    use type_string_utils, only: mono_type_to_string
    use codegen_type_utils, only: get_type_standardization
    implicit none
    private
    public :: infer_function_return_type_from_rhs
    public :: canonicalize_type
    public :: deduce_type_from_arguments

contains

    function infer_function_return_type_from_rhs(arena, stmt, defined_names, &
            defined_types, defined_count) &
            result(type_name)
        type(ast_arena_t), intent(in) :: arena
        type(assignment_node), intent(in) :: stmt
        character(len=*), intent(in) :: defined_names(:)
        character(len=*), intent(in) :: defined_types(:)
        integer, intent(in) :: defined_count
        character(len=:), allocatable :: type_name
        integer :: value_idx
        logical :: standardize_types_enabled

        call get_type_standardization(standardize_types_enabled)

        type_name = ""
        value_idx = stmt%value_index
        if (.not. arena%has_node_at(value_idx)) return

        select type (rhs => arena%entries(value_idx)%node)
            type is (call_or_subscript_node)
            if (len_trim(rhs%name) == 0) return
            type_name = lookup_function_return_type(defined_names, defined_types, &
                defined_count, rhs%name)
            type_name = canonicalize_type(type_name)
            if (len_trim(type_name) == 0) then
                type_name = deduce_type_from_arguments(arena, rhs, &
                    standardize_types_enabled)
            else
                block
                    character(len=:), allocatable :: inferred_type
                    character(len=:), allocatable :: current_lower
                    character(len=:), allocatable :: inferred_lower

                    current_lower = to_lower(trim(type_name))
                    if (current_lower == 'integer') then
                        inferred_type = deduce_type_from_arguments( &
                            arena, rhs, standardize_types_enabled)
                        inferred_lower = to_lower(trim(inferred_type))
                        if (len_trim(inferred_lower) > 0 .and. &
                            inferred_lower /= 'integer') then
                            type_name = inferred_type
                        end if
                    end if
                end block
            end if
            type is (array_literal_node)
            type_name = mono_type_to_string( &
                rhs%inferred_type, &
                include_shape=.true., &
                standardize_real=standardize_types_enabled, &
                fallback='')
        end select
    end function infer_function_return_type_from_rhs

    pure function canonicalize_type(type_str) result(canon)
        character(len=*), intent(in) :: type_str
        character(len=:), allocatable :: canon
        character(len=:), allocatable :: lowered

        lowered = to_lower(trim(type_str))
        if (len_trim(lowered) == 0) then
            canon = ""
            return
        end if

        select case (lowered)
        case ('double precision')
            canon = 'real(dp)'
        case default
            canon = trim(type_str)
        end select
    end function canonicalize_type

    function deduce_type_from_arguments(arena, call_node, standardize_real) &
            result(type_str)
        type(ast_arena_t), intent(in) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        logical, intent(in) :: standardize_real
        character(len=:), allocatable :: type_str
        integer :: i, arg_idx
        logical :: has_character
        logical :: has_complex
        logical :: has_double
        logical :: has_real
        logical :: has_logical
        logical :: has_integer
        character(len=:), allocatable :: arg_type
        character(len=:), allocatable :: lowered

        type_str = ""
        has_character = .false.
        has_complex = .false.
        has_double = .false.
        has_real = .false.
        has_logical = .false.
        has_integer = .false.

        if (.not. allocated(call_node%arg_indices)) return

        do i = 1, size(call_node%arg_indices)
            arg_idx = call_node%arg_indices(i)
            if (.not. arena%has_node_at(arg_idx)) cycle

            select type (arg_node => arena%entries(arg_idx)%node)
            class default
                arg_type = canonicalize_type(mono_type_to_string( &
                    arg_node%inferred_type, &
                    include_shape=.false., &
                    standardize_real=standardize_real, &
                    fallback=''))
            end select

            lowered = to_lower(trim(arg_type))
            if (len_trim(lowered) == 0) cycle
            if (index(lowered, 'character') == 1) then
                has_character = .true.
            else if (lowered == 'complex') then
                has_complex = .true.
            else if (lowered == 'logical') then
                has_logical = .true.
            else if (lowered == 'real(8)' .or. lowered == 'real(dp)') then
                has_double = .true.
            else if (index(lowered, 'real(') == 1) then
                has_double = .true.
            else if (lowered == 'real') then
                has_real = .true.
            else
                has_integer = .true.
            end if
        end do

        if (has_character) then
            type_str = 'character'
        else if (has_complex) then
            type_str = 'complex'
        else if (has_double) then
            type_str = 'real(dp)'
        else if (has_real) then
            type_str = 'real'
        else if (has_logical) then
            type_str = 'logical'
        else if (has_integer) then
            type_str = 'integer'
        end if
    end function deduce_type_from_arguments

    function lookup_function_return_type(func_names, func_types, count, &
            func_name) result(type_name)
        character(len=*), intent(in) :: func_names(:)
        character(len=*), intent(in) :: func_types(:)
        integer, intent(in) :: count
        character(len=*), intent(in) :: func_name
        character(len=:), allocatable :: type_name
        integer :: i

        type_name = ""
        if (len_trim(func_name) == 0) return

        do i = 1, count
            if (trim(func_names(i)) == trim(func_name)) then
                if (len_trim(func_types(i)) > 0) then
                    type_name = trim(func_types(i))
                end if
                return
            end if
        end do
    end function lookup_function_return_type
end module codegen_type_inference_utils
