module semantic_type_operations
    ! Type system operations extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
        substitution_t, &
        create_mono_type, create_type_var, &
        create_poly_type, compose_substitutions, &
        TVAR, TINT, TREAL, TCHAR, TLOGICAL, TDOUBLE
    use semantic_validation_utils, only: int_to_str
    implicit none
    private

    public :: generate_fresh_type_var_op
    public :: apply_substitution_to_type
    public :: generalize_type_op
    public :: instantiate_type_scheme_op
    public :: get_common_type

contains

    ! Generate fresh type variable (standalone version)
    function generate_fresh_type_var_op(next_var_id) result(tv)
        integer, intent(inout) :: next_var_id
        type(type_var_t) :: tv

        tv = create_type_var(next_var_id, "v"//int_to_str(next_var_id))
        next_var_id = next_var_id + 1
    end function generate_fresh_type_var_op

    ! Apply substitution to type (standalone version)
    function apply_substitution_to_type(typ, subst) result(result_type)
        type(mono_type_t), intent(in) :: typ
        type(substitution_t), intent(in) :: subst
        type(mono_type_t) :: result_type

        ! Simplified substitution application
        result_type = typ
    end function apply_substitution_to_type

    ! Generalize type (standalone version)
    function generalize_type_op(typ) result(scheme)
        type(mono_type_t), intent(in) :: typ
        type(poly_type_t) :: scheme
        type(type_var_t), allocatable :: free_vars(:)

        ! Simplified generalization
        allocate (free_vars(0))
        scheme = create_poly_type(free_vars, typ)
    end function generalize_type_op

    ! Instantiate type scheme (standalone version)
    function instantiate_type_scheme_op(scheme, next_var_id) result(typ)
        type(poly_type_t), intent(in) :: scheme
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ
        type(poly_type_t) :: temp_scheme

        if (scheme%mono_synced) then
            temp_scheme = scheme
            typ = temp_scheme%get_mono()
        else
            typ = create_mono_type(TVAR, var=generate_fresh_type_var_op(next_var_id))
        end if
    end function instantiate_type_scheme_op

    recursive function base_element_kind(typ) result(kind)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(in) :: typ
        integer :: kind
        type(mono_type_t) :: inner

        if (typ%kind /= TARRAY) then
            kind = typ%kind
            return
        end if

        if (typ%get_args_count() <= 0) then
            kind = typ%kind
            return
        end if

        inner = typ%get_arg(1)
        kind = base_element_kind(inner)
    end function base_element_kind

    ! Get common type for arithmetic operations
    function get_common_type(left_typ, right_typ) result(typ)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ
        integer :: target_kind
        integer :: left_base_kind
        integer :: right_base_kind
        logical :: needs_promotion

        left_base_kind = base_element_kind(left_typ)
        right_base_kind = base_element_kind(right_typ)

        ! Defer when an operand is an unresolved type variable (TVAR): the
        ! concrete result type depends on how the variable is eventually
        ! unified, so do not force a premature integer/real.  Otherwise an
        ! expression like `2*x` where x is later inferred real would lock the
        ! result to INTEGER while x is still a type variable (issue #2980).
        if (left_base_kind == TVAR) then
            typ = left_typ
            return
        end if
        if (right_base_kind == TVAR) then
            typ = right_typ
            return
        end if

        target_kind = left_base_kind
        if (left_base_kind == TDOUBLE .or. right_base_kind == TDOUBLE) then
            target_kind = TDOUBLE
        else if (left_base_kind == TREAL .or. right_base_kind == TREAL) then
            target_kind = TREAL
        else if (left_base_kind == TINT .or. right_base_kind == TINT) then
            target_kind = TINT
        end if

        needs_promotion = (target_kind /= left_base_kind) .or. &
            (target_kind /= right_base_kind)

        ! For array operations without promotion, return left array type directly
        if (left_typ%kind == TARRAY .and. .not. needs_promotion) then
            typ = left_typ
            return
        else if (right_typ%kind == TARRAY .and. .not. needs_promotion) then
            typ = right_typ
            return
        end if

        ! For scalar operations
        if (left_typ%kind /= TARRAY .and. right_typ%kind /= TARRAY) then
            if (needs_promotion) then
                typ = create_mono_type(target_kind)
            else
                typ = left_typ
            end if
            return
        end if

        ! For array operations with promotion, use promote function
        if (left_typ%kind == TARRAY) then
            typ = promote_array_element_type(left_typ, target_kind)
        else if (right_typ%kind == TARRAY) then
            typ = promote_array_element_type(right_typ, target_kind)
        else
            typ = create_mono_type(target_kind)
        end if
    end function get_common_type

    ! Recursively promote element type while preserving array structure
    recursive function promote_array_element_type(array_typ, target_kind) &
            result(promoted_typ)
        use type_system_unified, only: TARRAY
        type(mono_type_t), intent(in) :: array_typ
        integer, intent(in) :: target_kind
        type(mono_type_t) :: promoted_typ
        type(mono_type_t) :: inner_element, promoted_inner
        type(mono_type_t), allocatable :: promoted_args(:)

        if (array_typ%kind /= TARRAY) then
            ! Base case: non-array type, return promoted scalar
            promoted_typ = create_mono_type(target_kind)
            return
        end if

        ! Recursive case: get inner type and promote it
        if (array_typ%get_args_count() > 0) then
            inner_element = array_typ%get_arg(1)
            promoted_inner = promote_array_element_type(inner_element, target_kind)
            allocate (promoted_args(1))
            promoted_args(1) = promoted_inner
            promoted_typ = create_mono_type(TARRAY, &
                args=promoted_args, &
                array_size=array_typ%size)
        else
            ! Array without args (shouldn't happen, but handle gracefully)
            allocate (promoted_args(1))
            promoted_args(1) = create_mono_type(target_kind)
            promoted_typ = create_mono_type(TARRAY, &
                args=promoted_args, &
                array_size=array_typ%size)
        end if
    end function promote_array_element_type

end module semantic_type_operations
