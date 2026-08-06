module semantic_literal_identifier
    ! Literal and identifier type inference
    use type_system_unified, only: type_var_t, mono_type_t, &
        poly_type_t, create_mono_type, create_type_var, &
        create_poly_type, empty_type_vars, TVAR, TINT, TREAL, TCHAR, &
        TLOGICAL
    use scope_manager, only: scope_stack_t
    use error_handling, only: error_collection_t, result_t, &
        ERROR_SEMANTIC
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node
    use semantic_function_analysis, only: infer_type_from_usage_context
    use semantic_literal_type_helpers, only: infer_real_literal_kind
    use semantic_input_mode, only: INPUT_MODE_LAZY
    implicit none
    private

    public :: infer_literal_type
    public :: infer_identifier_type

contains

    function infer_literal_type(lit) result(typ)
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ

        select case (lit%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_REAL)
            typ = infer_real_literal_kind(lit)
        case (LITERAL_STRING)
            if (allocated(lit%value) .and. len(lit%value) >= 2) then
                ! Stored value retains its surrounding quotes; the character
                ! length is the interior, so subtract the two delimiters.
                typ = create_mono_type(TCHAR, char_size=len(lit%value) - 2)
            else
                typ = create_mono_type(TCHAR, char_size=0)
            end if
        case (LITERAL_LOGICAL)
            typ = create_mono_type(TLOGICAL)
        case default
            typ = create_mono_type(TREAL)
        end select
    end function infer_literal_type

    function infer_identifier_type(ident, scopes, errors, input_mode, next_var_id) &
            result(typ)
        type(identifier_node), intent(in) :: ident
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        integer, intent(in) :: input_mode
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ
        type(poly_type_t), allocatable :: scheme
        type(result_t) :: error_result

        if (.not. allocated(ident%name) .or. len_trim(ident%name) == 0) then
            typ = create_mono_type(TVAR, var=create_type_var(next_var_id, ""))
            next_var_id = next_var_id + 1
            return
        end if

        call scopes%lookup(ident%name, scheme)

        if (allocated(scheme)) then
            typ = instantiate_scheme_simple(scheme, next_var_id)
        else
            ! For now, don't error on undefined identifiers in STANDARD mode
            ! Standard Fortran files have explicit declarations and use statements
            ! We should trust them and just create type variables
            if (input_mode == INPUT_MODE_LAZY) then
                typ = infer_type_from_usage_context(ident%name, next_var_id)

                block
                    type(poly_type_t) :: new_scheme
                    new_scheme = create_poly_type(forall_vars=empty_type_vars(), &
                        mono=typ)
                    call scopes%define(ident%name, new_scheme)
                end block
            else
                ! STANDARD mode: create a type variable without error
                typ = create_mono_type(TVAR, var=create_type_var(next_var_id, ""))
                next_var_id = next_var_id + 1
            end if
        end if
    end function infer_identifier_type

    function instantiate_scheme_simple(scheme, next_var_id) result(typ)
        type(poly_type_t), intent(in) :: scheme
        integer, intent(inout) :: next_var_id
        type(mono_type_t) :: typ
        type(poly_type_t) :: mutable_scheme

        mutable_scheme = scheme
        typ = mutable_scheme%get_mono()
    end function instantiate_scheme_simple
end module semantic_literal_identifier
