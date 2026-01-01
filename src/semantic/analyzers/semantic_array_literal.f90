module semantic_array_literal
    ! Array literal type inference
    use, intrinsic :: iso_fortran_env, only: dp => real64
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   create_mono_type, create_poly_type, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL, TCOMPLEX, &
                                   TDOUBLE, TFUN, TARRAY, type_args_allocated, &
                                   type_args_size, type_args_element
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: array_literal_node
    use ast_nodes_loops, only: do_loop_node
    use string_utils_mod, only: to_lower
    use semantic_type_operations, only: get_common_type
    use standardizer_types, only: calculate_loop_size
    implicit none

    abstract interface
        function get_type_lookup_array_lit(a, idx) result(t)
            import :: mono_type_t, ast_arena_t
            type(ast_arena_t), intent(inout) :: a
            integer, intent(in) :: idx
            type(mono_type_t) :: t
        end function get_type_lookup_array_lit
    end interface

    private

    public :: infer_array_literal_type

contains

    function check_for_dynamic_implied_do(arena, array_lit) result(is_dynamic)
        type(ast_arena_t), intent(in) :: arena
        type(array_literal_node), intent(in) :: array_lit
        logical :: is_dynamic
        integer :: i
        integer :: loop_size

        is_dynamic = .false.

        if (.not. allocated(array_lit%element_indices)) return

        do i = 1, size(array_lit%element_indices)
            if (array_lit%element_indices(i) <= 0) cycle
            if (array_lit%element_indices(i) > arena%size) cycle
            if (.not. allocated(arena%entries(array_lit%element_indices(i))%node)) &
                cycle

            select type (elem => arena%entries(array_lit%element_indices(i))%node)
            type is (do_loop_node)
                loop_size = calculate_loop_size(arena, elem%start_expr_index, &
                                                elem%end_expr_index, &
                                                elem%step_expr_index)
                if (loop_size < 0) then
                    is_dynamic = .true.
                    return
                end if
            end select
        end do
    end function check_for_dynamic_implied_do

    function infer_array_literal_type(arena, array_lit, get_type_fn) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: array_lit
        procedure(get_type_lookup_array_lit) :: get_type_fn
        type(mono_type_t) :: typ
        type(mono_type_t) :: explicit_type
        type(mono_type_t) :: first_type
        type(mono_type_t) :: promoted_type
        logical :: all_arrays
        logical :: consistent_sizes
        logical :: has_real
        logical :: has_dynamic_implied_do
        integer :: elem_count
        integer :: first_array_size
        integer :: max_char_len

        explicit_type = parse_explicit_element_type(array_lit)

        if (.not. allocated(array_lit%element_indices) .or. &
            size(array_lit%element_indices) == 0) then
            typ = build_empty_array_type(explicit_type)
            return
        end if

        elem_count = size(array_lit%element_indices)

        if (explicit_type%kind > 0) then
            typ = build_explicit_array_type(explicit_type, elem_count)
            return
        end if

        has_dynamic_implied_do = check_for_dynamic_implied_do(arena, array_lit)

        call promote_array_element_types(arena, array_lit, get_type_fn, &
                                         first_type, promoted_type, &
                                         all_arrays, consistent_sizes, &
                                         has_real, &
                                         max_char_len, first_array_size)

        typ = build_array_type_from_elements(elem_count, first_type, &
                                             promoted_type, all_arrays, &
                                             consistent_sizes, has_real, &
                                             max_char_len, first_array_size, &
                                             has_dynamic_implied_do)
    end function infer_array_literal_type

    function parse_explicit_element_type(array_lit) result(explicit_type)
        type(array_literal_node), intent(in) :: array_lit
        type(mono_type_t) :: explicit_type
        character(len=:), allocatable :: explicit_spec

        explicit_type%kind = 0
        explicit_type%size = 0

        if (.not. allocated(array_lit%type_spec)) return

        explicit_spec = trim(array_lit%type_spec)
        if (len_trim(explicit_spec) == 0) return

        explicit_type = mono_type_from_type_spec(explicit_spec)
    end function parse_explicit_element_type

    function build_empty_array_type(explicit_type) result(array_type)
        type(mono_type_t), intent(in) :: explicit_type
        type(mono_type_t) :: array_type
        type(mono_type_t) :: element_type
        type(mono_type_t), allocatable :: args(:)

        element_type = explicit_type
        if (element_type%kind <= 0) then
            element_type = create_mono_type(TINT)
        end if

        allocate (args(1))
        args(1) = element_type

        array_type = create_mono_type(TARRAY, args=args)
        array_type%size = 0
        array_type%alloc_info%is_pointer = .false.
        array_type%alloc_info%needs_allocatable_string = .false.

        if (explicit_type%kind > 0) then
            array_type%alloc_info%is_allocatable = .false.
            array_type%alloc_info%needs_allocation_check = .false.
        else
            array_type%alloc_info%is_allocatable = .true.
            array_type%alloc_info%needs_allocation_check = .true.
        end if
    end function build_empty_array_type

    function build_explicit_array_type(explicit_type, element_count) &
        result(array_type)
        type(mono_type_t), intent(in) :: explicit_type
        integer, intent(in) :: element_count
        type(mono_type_t) :: array_type
        type(mono_type_t), allocatable :: args(:)

        allocate (args(1))
        args(1) = explicit_type
        array_type = create_mono_type(TARRAY, args=args, &
                                      array_size=element_count)
    end function build_explicit_array_type

    subroutine promote_array_element_types(arena, array_lit, get_type_fn, &
                                           first_type, promoted_type, &
                                           all_arrays, consistent_sizes, &
                                           has_real, max_char_len, &
                                           first_array_size)
        type(ast_arena_t), intent(inout) :: arena
        type(array_literal_node), intent(in) :: array_lit
        procedure(get_type_lookup_array_lit) :: get_type_fn
        type(mono_type_t), intent(out) :: first_type
        type(mono_type_t), intent(out) :: promoted_type
        logical, intent(out) :: all_arrays
        logical, intent(out) :: consistent_sizes
        logical, intent(out) :: has_real
        integer, intent(out) :: max_char_len
        integer, intent(out) :: first_array_size
        type(mono_type_t) :: element_type
        integer :: i
        integer :: elem_array_size

        first_type = resolve_constructor_element_type( &
                     arena, array_lit%element_indices(1), get_type_fn)
        promoted_type = first_type
        has_real = (first_type%kind == TREAL)
        all_arrays = (first_type%kind == TARRAY)
        consistent_sizes = .true.
        max_char_len = 0
        first_array_size = 0

        if (all_arrays) first_array_size = first_type%size
        if (first_type%kind == TCHAR) max_char_len = first_type%size

        do i = 2, size(array_lit%element_indices)
            element_type = resolve_constructor_element_type( &
                           arena, array_lit%element_indices(i), get_type_fn)

            if (all_arrays .and. element_type%kind /= TARRAY) then
                all_arrays = .false.
            else if (all_arrays .and. element_type%kind == TARRAY) then
                elem_array_size = element_type%size
                if (elem_array_size /= first_array_size) then
                    consistent_sizes = .false.
                end if
            end if

            if (element_type%kind == TCHAR) then
                max_char_len = max(max_char_len, element_type%size)
            end if

            if (element_type%kind == TREAL) then
                has_real = .true.
                if (.not. all_arrays) promoted_type = create_mono_type(TREAL)
            else if (element_type%kind == TARRAY .and. &
                     element_type%has_args()) then
                if (element_type%get_args_count() > 0) then
                    promoted_type = element_type%get_arg(1)
                    if (promoted_type%kind == TREAL) then
                        has_real = .true.
                    end if
                end if
            end if
        end do
    end subroutine promote_array_element_types

    function build_array_type_from_elements(element_count, first_type, &
                                            promoted_type, all_arrays, &
                                            consistent_sizes, has_real, &
                                            max_char_len, first_array_size, &
                                            has_dynamic_implied_do) &
        result(array_type)
        integer, intent(in) :: element_count
        type(mono_type_t), intent(in) :: first_type
        type(mono_type_t), intent(in) :: promoted_type
        logical, intent(in) :: all_arrays
        logical, intent(in) :: consistent_sizes
        logical, intent(in) :: has_real
        integer, intent(in) :: max_char_len
        integer, intent(in) :: first_array_size
        logical, intent(in) :: has_dynamic_implied_do
        type(mono_type_t) :: array_type
        type(mono_type_t) :: result_promoted
        type(mono_type_t), allocatable :: args(:)
        type(mono_type_t), allocatable :: inner_args(:)

        result_promoted = promoted_type

        if (has_dynamic_implied_do) then
            if (has_real .and. result_promoted%kind == TINT) then
                result_promoted = create_mono_type(TREAL)
            end if

            if (result_promoted%kind == TCHAR .and. max_char_len > 0) then
                result_promoted = create_mono_type(TCHAR, &
                                                   char_size=max_char_len)
            end if

            allocate (args(1))
            args(1) = result_promoted
            array_type = create_mono_type(TARRAY, args=args)
            array_type%alloc_info%is_allocatable = .true.
            array_type%alloc_info%needs_allocation_check = .true.
            array_type%alloc_info%is_pointer = .false.
            array_type%alloc_info%needs_allocatable_string = .false.
        else if (all_arrays .and. consistent_sizes) then
            if (first_type%has_args() .and. &
                first_type%get_args_count() > 0) then
                if (has_real) then
                    result_promoted = create_mono_type(TREAL)
                else
                    result_promoted = first_type%get_arg(1)
                end if
            else
                result_promoted = create_mono_type(TINT)
            end if

            allocate (inner_args(1))
            inner_args(1) = result_promoted

            allocate (args(1))
            args(1) = create_mono_type(TARRAY, args=inner_args, &
                                       array_size=first_array_size)
            array_type = create_mono_type(TARRAY, args=args, &
                                          array_size=element_count)
            deallocate (inner_args)
        else
            if (has_real .and. result_promoted%kind == TINT) then
                result_promoted = create_mono_type(TREAL)
            end if

            if (result_promoted%kind == TCHAR .and. max_char_len > 0) then
                result_promoted = create_mono_type(TCHAR, &
                                                   char_size=max_char_len)
            end if

            allocate (args(1))
            args(1) = result_promoted
            array_type = create_mono_type(TARRAY, args=args, &
                                          array_size=element_count)
        end if
    end function build_array_type_from_elements

    recursive function resolve_constructor_element_type(arena, element_index, &
                                                        get_type_fn) &
        result(resolved_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: element_index
        procedure(get_type_lookup_array_lit) :: get_type_fn
        type(mono_type_t) :: resolved_type

        resolved_type%kind = 0
        resolved_type%size = 0
        if (element_index <= 0) return
        if (element_index > arena%size) return
        if (.not. allocated(arena%entries(element_index)%node)) return

        select type (element_node => arena%entries(element_index)%node)
        type is (do_loop_node)
            resolved_type = resolve_implied_do_type(arena, element_index, &
                                                    get_type_fn)
        class default
            resolved_type = get_type_fn(arena, element_index)
        end select
    end function resolve_constructor_element_type

    recursive function resolve_implied_do_type(arena, loop_index, &
                                               get_type_fn) &
        result(resolved_type)
        type(ast_arena_t), intent(inout) :: arena
        integer, intent(in) :: loop_index
        procedure(get_type_lookup_array_lit) :: get_type_fn
        type(mono_type_t) :: resolved_type
        type(mono_type_t) :: element_type
        integer :: i

        resolved_type%kind = 0
        resolved_type%size = 0
        if (loop_index <= 0) return
        if (loop_index > arena%size) return
        if (.not. allocated(arena%entries(loop_index)%node)) return

        select type (loop_node => arena%entries(loop_index)%node)
        type is (do_loop_node)
            if (.not. allocated(loop_node%body_indices)) return
            do i = 1, size(loop_node%body_indices)
                element_type = resolve_constructor_element_type( &
                               arena, loop_node%body_indices(i), get_type_fn)
                if (element_type%kind == 0) cycle
                if (resolved_type%kind == 0) then
                    resolved_type = element_type
                else
                    resolved_type = get_common_type(resolved_type, &
                                                    element_type)
                end if
            end do
            if (resolved_type%kind == 0) resolved_type = create_mono_type(TINT)
        class default
            resolved_type = get_type_fn(arena, loop_index)
        end select
    end function resolve_implied_do_type

    function mono_type_from_type_spec(type_spec) result(explicit_type)
        character(len=*), intent(in) :: type_spec
        type(mono_type_t) :: explicit_type
        character(len=:), allocatable :: trimmed
        character(len=:), allocatable :: lowered
        integer :: char_len

        explicit_type%kind = 0
        explicit_type%size = 0

        trimmed = adjustl(trim(type_spec))
        if (len_trim(trimmed) == 0) return

        lowered = to_lower(trimmed)

        if (index(lowered, 'double precision') == 1) then
            explicit_type = create_mono_type(TDOUBLE)
            return
        else if (index(lowered, 'integer') == 1) then
            explicit_type = create_mono_type(TINT)
            return
        else if (index(lowered, 'real') == 1) then
            explicit_type = create_mono_type(TREAL)
            return
        else if (index(lowered, 'logical') == 1) then
            explicit_type = create_mono_type(TLOGICAL)
            return
        else if (index(lowered, 'complex') == 1) then
            explicit_type = create_mono_type(TCOMPLEX)
            return
        else if (index(lowered, 'character') == 1) then
            char_len = extract_first_integer(trimmed)
            if (char_len > 0) then
                explicit_type = create_mono_type(TCHAR, char_size=char_len)
            else
                explicit_type = create_mono_type(TCHAR)
            end if
            return
        end if
    end function mono_type_from_type_spec

    pure function extract_first_integer(text) result(value)
        character(len=*), intent(in) :: text
        integer :: value
        integer :: i
        integer :: buf_len
        character(len=32) :: buffer
        integer :: ios

        value = -1
        buf_len = 0
        buffer = ' '

        do i = 1, len_trim(text)
            if (text(i:i) >= '0' .and. text(i:i) <= '9') then
                if (buf_len < len(buffer)) then
                    buf_len = buf_len + 1
                    buffer(buf_len:buf_len) = text(i:i)
                end if
            else if (buf_len > 0) then
                exit
            end if
        end do

        if (buf_len > 0) then
            read (buffer(1:buf_len), *, iostat=ios) value
            if (ios /= 0) value = -1
        end if
    end function extract_first_integer

end module semantic_array_literal
