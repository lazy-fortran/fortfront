module semantic_literal_identifier
    ! Literal and identifier type inference
    use type_system_unified, only: type_env_t, type_var_t, mono_type_t, &
                                   poly_type_t, create_mono_type, create_type_var, &
                                   create_poly_type, TVAR, TINT, TREAL, TCHAR, &
                                   TLOGICAL, TDOUBLE
    use scope_manager, only: scope_stack_t
    use error_handling, only: error_collection_t, result_t, create_error_result, &
                              ERROR_SEMANTIC
    use ast_base, only: LITERAL_INTEGER, LITERAL_REAL, LITERAL_STRING, LITERAL_LOGICAL
    use ast_nodes_core, only: literal_node, identifier_node
    use semantic_function_analysis, only: infer_type_from_usage_context
    use string_utils_mod, only: to_lower
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
            typ = infer_real_literal_type(lit)
        case (LITERAL_STRING)
            if (allocated(lit%value) .and. len(lit%value) >= 2) then
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

    function infer_identifier_type(ident, scopes, errors, strict_mode, next_var_id) &
        result(typ)
        type(identifier_node), intent(in) :: ident
        type(scope_stack_t), intent(inout) :: scopes
        type(error_collection_t), intent(inout) :: errors
        logical, intent(in) :: strict_mode
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
            if (strict_mode) then
                error_result = create_error_result( &
                               "Undefined variable '"//ident%name//"' in strict mode", &
                               ERROR_SEMANTIC, &
                               component="semantic_literal_identifier", &
                               context="infer_identifier_type", &
                               suggestion="Declare 'integer :: "//ident%name// &
                               "' or drop 'implicit none' for lazy Fortran mode")
                call errors%add_result(error_result)

                typ = create_mono_type(TVAR, var=create_type_var(next_var_id, ""))
                next_var_id = next_var_id + 1
            else
                typ = infer_type_from_usage_context(ident%name, next_var_id)

                block
                    type(poly_type_t) :: new_scheme
                    new_scheme = create_poly_type(forall_vars=[type_var_t &
                                                               ::], mono=typ)
                    call scopes%define(ident%name, new_scheme)
                end block
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

    function infer_real_literal_type(lit) result(typ)
        type(literal_node), intent(in) :: lit
        type(mono_type_t) :: typ
        character(len=:), allocatable :: literal_value
        character(len=:), allocatable :: lowered_value
        character(len=:), allocatable :: kind_token
        integer :: underscore_pos
        integer :: read_status
        integer :: kind_int

        typ = create_mono_type(TREAL)
        if (.not. allocated(lit%value)) return

        literal_value = trim(lit%value)
        if (len(literal_value) == 0) return

        lowered_value = to_lower(literal_value)
        if (contains_double_exponent(lowered_value)) then
            typ = create_mono_type(TDOUBLE)
            return
        end if

        underscore_pos = index(lowered_value, "_")
        if (underscore_pos <= 0) return
        if (underscore_pos == len(lowered_value)) return

        kind_token = adjustl(lowered_value(underscore_pos + 1:))
        kind_token = trim(kind_token)
        if (len(kind_token) == 0) return

        select case (kind_token)
        case ("real64", "double", "doubleprecision", "dp")
            typ = create_mono_type(TDOUBLE)
            return
        case ("real32", "sp")
            typ = create_mono_type(TREAL)
            return
        case default
            read (kind_token, *, iostat=read_status) kind_int
            if (read_status /= 0) return
            if (kind_int >= 8) then
                typ = create_mono_type(TDOUBLE)
            else
                typ = create_mono_type(TREAL)
            end if
        end select
    end function infer_real_literal_type

    pure logical function contains_double_exponent(text) result(has_double)
        character(len=*), intent(in) :: text
        integer :: i
        integer :: trimmed_length

        has_double = .false.
        trimmed_length = len_trim(text)

        do i = 1, trimmed_length
            if (text(i:i) /= 'd') cycle
            if (i <= 1) cycle
            if (.not. is_real_digit_or_dot(text(i - 1:i - 1))) cycle
            if (i == trimmed_length) then
                has_double = .true.
                return
            end if
            if (.not. is_digit_or_sign(text(i + 1:i + 1))) cycle
            has_double = .true.
            return
        end do
    end function contains_double_exponent

    pure logical function is_real_digit_or_dot(ch) result(is_valid)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_valid = (ch == '.') .or. (code >= iachar('0') .and. code <= iachar('9'))
    end function is_real_digit_or_dot

    pure logical function is_digit_or_sign(ch) result(is_valid)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        is_valid = (code >= iachar('0') .and. code <= iachar('9')) .or. ch == '+' &
                   .or. ch == '-'
    end function is_digit_or_sign

end module semantic_literal_identifier
