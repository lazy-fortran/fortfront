module semantic_type_operations
    ! Type system operations extracted from semantic_analyzer
    ! for architectural compliance (Issue #1117)
    use type_system_unified, only: type_var_t, mono_type_t, poly_type_t, &
                                   substitution_t, &
                                   create_mono_type, create_type_var, &
                                   create_poly_type, compose_substitutions, &
                                   TVAR, TINT, TREAL, TCHAR, TLOGICAL
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
        allocate(free_vars(0))
        scheme = create_poly_type(free_vars, typ)
    end function generalize_type_op

    ! Instantiate type scheme (standalone version)
    function instantiate_type_scheme_op(scheme, next_var_id) result(typ)
        type(poly_type_t), intent(in) :: scheme
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ

        ! Simplified instantiation
        typ = create_mono_type(TVAR, var=generate_fresh_type_var_op(next_var_id))
    end function instantiate_type_scheme_op

    ! Get common type for arithmetic operations
    function get_common_type(left_typ, right_typ) result(typ)
        type(mono_type_t), intent(in) :: left_typ, right_typ
        type(mono_type_t) :: typ
        
        ! Type promotion rules: real > integer > other
        if (left_typ%kind == TREAL .or. right_typ%kind == TREAL) then
            typ = create_mono_type(TREAL)
        else if (left_typ%kind == TINT .or. right_typ%kind == TINT) then
            typ = create_mono_type(TINT)
        else
            ! Default to left operand type
            typ = left_typ
        end if
    end function get_common_type

end module semantic_type_operations