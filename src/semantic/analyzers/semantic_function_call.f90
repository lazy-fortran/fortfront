module semantic_function_call
    use type_system_unified, only: mono_type_t, poly_type_t, create_mono_type, &
                                   TREAL, TINT, TCHAR, TLOGICAL, TFUN, TARRAY, &
                                   TCOMPLEX, type_args_allocated, type_args_size, &
                                   type_args_element
    use scope_manager, only: scope_stack_t
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: call_or_subscript_node
    use intrinsic_registry, only: get_intrinsic_signature, &
                                  is_intrinsic_function
    use string_utils_mod, only: to_lower
    use semantic_array_type_builders, only: collapse_array_rank
    use semantic_array_intrinsics, only: infer_array_intrinsic_type
    use semantic_function_helpers, only: get_type_lookup, &
                                         gather_call_argument_types, &
                                         refine_type_from_arguments, &
                                         refine_character_intrinsic_result, &
                                         find_return_type
    implicit none
    private

    public :: infer_function_call_type

contains

    function infer_function_call_type(arena, call_node, scopes, get_type_fn) &
        result(typ)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: call_node
        type(scope_stack_t), intent(inout) :: scopes
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t) :: typ
        character(len=:), allocatable :: lowered_name
        logical :: is_intrinsic_func
        logical :: has_function_scheme
        logical :: return_type_locked
        type(mono_type_t), allocatable :: arg_types(:)

        typ = create_mono_type(TREAL)
        is_intrinsic_func = .false.
        has_function_scheme = .false.

        call gather_call_argument_types(arena, call_node, get_type_fn, &
                                        arg_types)

        if (allocated(call_node%name)) then
            lowered_name = to_lower(trim(call_node%name))
        else
            lowered_name = ""
        end if

        call resolve_function_call_type(arena, call_node, scopes, get_type_fn, &
                                        typ, is_intrinsic_func, &
                                        has_function_scheme, &
                                        return_type_locked)

        if (is_intrinsic_func) then
            call refine_character_intrinsic_result(lowered_name, &
                                                   arg_types, typ)
        end if

        call apply_array_access_rules(call_node, has_function_scheme, &
                                      is_intrinsic_func, typ)

        call refine_type_from_arguments(arg_types, typ, return_type_locked, &
                                        is_intrinsic_func)

        if (is_intrinsic_func) then
            select case (lowered_name)
            case ("count")
                typ = create_mono_type(TINT)
            case ("any", "all")
                typ = create_mono_type(TLOGICAL)
            end select
        end if
    end function infer_function_call_type

    subroutine resolve_function_call_type(arena, call_node, scopes, &
                                          get_type_fn, typ, &
                                          is_intrinsic_func, &
                                          has_function_scheme, &
                                          return_type_locked)
        type(ast_arena_t), intent(inout) :: arena
        type(call_or_subscript_node), intent(inout) :: call_node
        type(scope_stack_t), intent(inout) :: scopes
        procedure(get_type_lookup) :: get_type_fn
        type(mono_type_t), intent(inout) :: typ
        logical, intent(out) :: is_intrinsic_func
        logical, intent(out) :: has_function_scheme
        logical, intent(out) :: return_type_locked
        type(poly_type_t), allocatable :: scheme
        type(mono_type_t) :: scheme_mono
        character(len=:), allocatable :: intrinsic_sig

        is_intrinsic_func = .false.
        has_function_scheme = .false.
        return_type_locked = .false.

        if (allocated(call_node%name)) then
            call scopes%lookup(call_node%name, scheme)
        end if

        if (allocated(scheme)) then
            scheme_mono = scheme%get_mono()
            has_function_scheme = (scheme_mono%kind == TFUN)
            typ = scheme_mono
            if (has_function_scheme .and. type_args_allocated(typ) .and. &
                type_args_size(typ) >= 2) then
                typ = type_args_element(typ, 2)
            end if
            return_type_locked = has_function_scheme
            return
        end if

        if (.not. allocated(call_node%name)) then
            typ = create_mono_type(TREAL)
            return
        end if

        if (find_return_type(arena, call_node%name, typ)) then
            has_function_scheme = .true.
            return_type_locked = .true.
            return
        end if

        is_intrinsic_func = is_intrinsic_function(call_node%name)

        if (.not. is_intrinsic_func) then
            typ = create_mono_type(TREAL)
            return_type_locked = .false.
            return
        end if

        intrinsic_sig = get_intrinsic_signature(call_node%name)

        if (len_trim(intrinsic_sig) <= 0) then
            typ = create_mono_type(TREAL)
            return
        end if

        if (index(intrinsic_sig, "real(") == 1) then
            typ = create_mono_type(TREAL)
            return_type_locked = .true.
        else if (index(intrinsic_sig, "integer(") == 1) then
            typ = create_mono_type(TINT)
            return_type_locked = .true.
        else if (index(intrinsic_sig, "unsigned_integer(") == 1) then
            typ = create_mono_type(TINT, is_unsigned=.true.)
            return_type_locked = .true.
        else if (index(intrinsic_sig, "logical(") == 1) then
            typ = create_mono_type(TLOGICAL)
            return_type_locked = .true.
        else if (index(intrinsic_sig, "character(") == 1) then
            typ = create_mono_type(TCHAR)
            return_type_locked = .true.
        else if (index(intrinsic_sig, "complex(") == 1) then
            typ = create_mono_type(TCOMPLEX)
            return_type_locked = .true.
        else if (index(intrinsic_sig, "array(") == 1) then
            typ = infer_array_intrinsic_type(arena, call_node, get_type_fn)
            return_type_locked = .false.
        else
            typ = create_mono_type(TREAL)
            return_type_locked = .false.
        end if
    end subroutine resolve_function_call_type

    subroutine apply_array_access_rules(call_node, has_function_scheme, &
                                        is_intrinsic_func, typ)
        type(call_or_subscript_node), intent(inout) :: call_node
        logical, intent(in) :: has_function_scheme
        logical, intent(in) :: is_intrinsic_func
        type(mono_type_t), intent(inout) :: typ
        integer :: subscript_rank
        logical :: treat_as_array_access
        type(mono_type_t) :: base_array_type

        subscript_rank = 0
        if (allocated(call_node%arg_indices)) subscript_rank = &
            size(call_node%arg_indices)

        treat_as_array_access = call_node%is_array_access
        if (.not. treat_as_array_access) then
            treat_as_array_access = subscript_rank > 0 .and. &
                                    .not. has_function_scheme .and. &
                                    .not. is_intrinsic_func
        end if

        if (treat_as_array_access .and. typ%kind == TARRAY) then
            base_array_type = collapse_array_rank(typ, subscript_rank)
            if (base_array_type%kind == 0) base_array_type = typ
            typ = base_array_type
            call_node%is_array_access = .true.
        end if
    end subroutine apply_array_access_rules

end module semantic_function_call
