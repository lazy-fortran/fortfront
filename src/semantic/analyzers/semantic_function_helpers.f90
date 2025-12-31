module semantic_function_helpers
    use type_system_unified, only: mono_type_t, create_mono_type, &
                                   TINT, TREAL, TCHAR, TLOGICAL, TDOUBLE, &
                                   TCOMPLEX, TFUN, TARRAY, TVAR, &
                                   type_args_allocated, type_args_size, &
                                   type_args_element
    use type_array_safe, only: safe_peel_array_to_base
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node
    use ast_nodes_procedure, only: function_def_node
    implicit none
    private

    abstract interface
        function get_type_lookup(a, idx) result(t)
            import :: mono_type_t, ast_arena_t
            type(ast_arena_t), intent(inout) :: a
            integer, intent(in) :: idx
            type(mono_type_t) :: t
        end function get_type_lookup
    end interface

    public :: get_type_lookup
    public :: gather_call_argument_types
    public :: refine_type_from_arguments
    public :: refine_character_intrinsic_result
    public :: find_return_type

contains

    subroutine gather_call_argument_types(arena, call_node, get_type_fn, &
                                          arg_types)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(in) :: call_node
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t), allocatable, intent(out) :: arg_types(:)
        integer :: i

        if (allocated(call_node%arg_indices)) then
            allocate (arg_types(size(call_node%arg_indices)))
            do i = 1, size(call_node%arg_indices)
                arg_types(i) = get_type_fn(arena, call_node%arg_indices(i))
            end do
        else
            allocate (arg_types(0))
        end if
    end subroutine gather_call_argument_types

    subroutine refine_type_from_arguments(arg_types, typ, type_locked, &
                                          is_intrinsic)
        type(mono_type_t), intent(in) :: arg_types(:)
        type(mono_type_t), intent(inout) :: typ
        logical, intent(in), optional :: type_locked
        logical, intent(in), optional :: is_intrinsic
        logical :: allow_refinement
        integer :: deduced_kind
        logical :: has_array_arg
        type(mono_type_t), allocatable :: element_args(:)
        integer :: i
        logical :: skip_array_inference

        if (size(arg_types) <= 0) return

        ! For intrinsic functions, do NOT apply array-in-array-out inference.
        ! Intrinsics like sum, maxval, etc. return scalars from arrays.
        ! Their return types are already correctly computed by the intrinsic
        ! handlers in semantic_array_intrinsics.f90 or intrinsic_registry.
        skip_array_inference = .false.
        if (present(is_intrinsic)) skip_array_inference = is_intrinsic

        ! Check if any argument is an array FIRST (Issue #2153)
        ! For elementwise operations, array input means array output
        ! This check must happen BEFORE type_locked is checked because
        ! array-in-array-out is fundamental and overrides type locking
        has_array_arg = .false.
        if (.not. skip_array_inference) then
            do i = 1, size(arg_types)
                if (arg_types(i)%kind == TARRAY) then
                    has_array_arg = .true.
                    exit
                end if
            end do
        end if

        deduced_kind = deduce_return_kind_from_args(arg_types)
        if (deduced_kind <= 0) return

        ! If we have array arguments, create array return type (Issue #2153)
        ! This MUST happen regardless of type_locked because the locked type
        ! was inferred from a generic function signature, not this specific
        ! call with array arguments
        ! However, do NOT override existing array types (e.g., matmul result
        ! which has correct rank computed by semantic_array_intrinsics)
        if (has_array_arg .and. typ%kind /= TARRAY) then
            allocate (element_args(1))
            element_args(1) = create_mono_type(deduced_kind)
            typ = create_mono_type(TARRAY, args=element_args)
            typ%alloc_info%is_allocatable = .true.
            deallocate (element_args)
            return
        end if

        ! For scalar arguments, respect the type_locked flag
        allow_refinement = .true.
        if (present(type_locked)) allow_refinement = .not. type_locked
        if (.not. allow_refinement) return

        select case (typ%kind)
        case (TVAR)
            typ = create_mono_type(deduced_kind)
        case (TREAL)
            if (deduced_kind /= TREAL) typ = create_mono_type(deduced_kind)
        case (TINT)
            if (deduced_kind /= TINT) typ = create_mono_type(deduced_kind)
        case default
            if (typ%kind <= 0) typ = create_mono_type(deduced_kind)
        end select
    end subroutine refine_type_from_arguments

    subroutine refine_character_intrinsic_result(name, arg_types, typ)
        character(len=*), intent(in) :: name
        type(mono_type_t), intent(in) :: arg_types(:)
        type(mono_type_t), intent(inout) :: typ
        type(mono_type_t) :: arg_copy
        integer :: arg_len

        if (name /= "trim" .and. name /= "adjustl" .and. name /= &
            "adjustr") return
        if (size(arg_types) <= 0) return

        arg_copy = arg_types(1)
        call arg_copy%sync_from_arena()
        if (arg_copy%kind /= TCHAR) then
            typ = create_mono_type(TCHAR)
            typ%alloc_info%needs_allocatable_string = .true.
            return
        end if

        arg_len = max(arg_copy%size, 0)
        if (arg_copy%alloc_info%needs_allocatable_string) then
            typ = create_mono_type(TCHAR)
            typ%alloc_info%needs_allocatable_string = .true.
        else
            typ = create_mono_type(TCHAR, char_size=arg_len)
            typ%alloc_info%needs_allocatable_string = .false.
        end if
    end subroutine refine_character_intrinsic_result

    integer function deduce_return_kind_from_args(arg_types) result(kind_value)
        type(mono_type_t), intent(in) :: arg_types(:)
        integer :: i
        integer :: best_kind
        integer :: current_kind

        best_kind = 0
        do i = 1, size(arg_types)
            block
                type(mono_type_t) :: type_copy
                type_copy = safe_peel_array_to_base(arg_types(i))
                current_kind = type_copy%kind
            end block
            if (current_kind <= 0) cycle
            select case (current_kind)
            case (TDOUBLE)
                kind_value = TDOUBLE
                return
            case (TCOMPLEX)
                if (best_kind /= TDOUBLE) best_kind = TCOMPLEX
            case (TREAL)
                if (best_kind /= TDOUBLE .and. best_kind /= TCOMPLEX) &
                    best_kind = TREAL
            case (TCHAR)
                if (best_kind == 0) best_kind = TCHAR
            case (TLOGICAL)
                if (best_kind == 0) best_kind = TLOGICAL
            case (TINT)
                if (best_kind == 0) best_kind = TINT
            case default
                if (best_kind == 0) best_kind = current_kind
            end select
        end do

        kind_value = best_kind
    end function deduce_return_kind_from_args

    logical function find_return_type(arena, func_name, return_type) &
        result(found)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: func_name
        type(mono_type_t), intent(out) :: return_type
        integer :: i

        found = .false.
        return_type = create_mono_type(TREAL)

        do i = 1, arena%size
            if (.not. allocated(arena%entries(i)%node)) cycle
            select type (node => arena%entries(i)%node)
            type is (function_def_node)
                if (.not. allocated(node%name)) cycle
                if (trim(node%name) /= trim(func_name)) cycle
                if (node%inferred_type%kind == TFUN .and. &
                    type_args_allocated(node%inferred_type) .and. &
                    type_args_size(node%inferred_type) >= 2) then
                    return_type = type_args_element(node%inferred_type, 2)
                    found = .true.
                    return
                else if (allocated(node%return_type)) then
                    select case (trim(node%return_type))
                    case ("integer")
                        return_type = create_mono_type(TINT)
                    case ("logical")
                        return_type = create_mono_type(TLOGICAL)
                    case ("character")
                        return_type = create_mono_type(TCHAR)
                    case default
                        return_type = create_mono_type(TREAL)
                    end select
                    found = .true.
                    return
                end if
            end select
        end do
    end function find_return_type

end module semantic_function_helpers
