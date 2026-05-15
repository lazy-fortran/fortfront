module semantic_strict_argument_type_checker_types
    use ast_base, only: LITERAL_INTEGER, LITERAL_LOGICAL, LITERAL_REAL, &
                        LITERAL_STRING
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: assignment_node, literal_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node
    use semantic_procedure_utils, only: declaration_type_to_mono
    use string_utils_mod, only: to_lower
    use type_system_unified, only: mono_type_t, create_mono_type, TARRAY, TDOUBLE, &
                                   TCHAR, TCOMPLEX, TINT, TLOGICAL, TREAL, TVAR
    implicit none
    private

    public :: strict_actual_argument_type
    public :: strict_dummy_type
    public :: strict_type_is_known
    public :: strict_type_name
    public :: strict_types_match

contains

    function strict_dummy_type(arena, decl_index) result(typ)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: decl_index
        type(mono_type_t) :: typ
        character(len=:), allocatable :: lowered

        typ%kind = 0
        if (.not. arena%has_node_at(decl_index)) return

        select type (node => arena%entries(decl_index)%node)
        type is (parameter_declaration_node)
            if (.not. allocated(node%type_name)) return
            lowered = to_lower(trim(node%type_name))
            if (lowered == "real" .and. node%has_kind) then
                if (node%kind_value == 8) then
                    typ = create_mono_type(TDOUBLE)
                    return
                end if
            end if
            typ = declaration_type_to_mono(node%type_name)
            if (node%is_unsigned) then
                if (typ%kind == TINT) then
                    typ%is_unsigned = .true.
                end if
            end if
        type is (declaration_node)
            if (allocated(node%type_name)) then
                typ = declaration_type_to_mono(node%type_name)
            else
                typ = node%inferred_type
            end if
            if (node%is_unsigned) then
                if (typ%kind == TINT) then
                    typ%is_unsigned = .true.
                end if
            end if
        class default
            typ = arena%entries(decl_index)%node%inferred_type
        end select
    end function strict_dummy_type

    recursive function strict_actual_argument_type(arena, expr_index) result(typ)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: expr_index
        type(mono_type_t) :: typ

        typ%kind = 0
        if (.not. arena%has_node_at(expr_index)) return

        select type (node => arena%entries(expr_index)%node)
        type is (literal_node)
            typ = strict_literal_type(node)
        type is (assignment_node)
            if (node%is_keyword_argument) then
                typ = strict_actual_argument_type(arena, node%value_index)
            else
                typ = node%inferred_type
            end if
        class default
            typ = arena%entries(expr_index)%node%inferred_type
        end select
    end function strict_actual_argument_type

    function strict_literal_type(node) result(typ)
        type(literal_node), intent(in) :: node
        type(mono_type_t) :: typ
        character(len=:), allocatable :: lowered
        integer :: kind_value

        typ%kind = 0
        if (.not. allocated(node%value)) return

        select case (node%literal_kind)
        case (LITERAL_INTEGER)
            typ = create_mono_type(TINT)
        case (LITERAL_LOGICAL)
            typ = create_mono_type(TLOGICAL)
        case (LITERAL_STRING)
            typ = create_mono_type(TCHAR)
        case (LITERAL_REAL)
            lowered = to_lower(trim(node%value))
            kind_value = parse_literal_kind_suffix(lowered)
            if (index(lowered, 'd') > 0 .or. kind_value == 8) then
                typ = create_mono_type(TDOUBLE)
            else
                typ = create_mono_type(TREAL)
            end if
        case default
            typ%kind = 0
        end select
    end function strict_literal_type

    pure integer function parse_literal_kind_suffix(text) result(kind_value)
        character(len=*), intent(in) :: text
        integer :: underscore_pos
        integer :: ios
        character(len=:), allocatable :: suffix

        kind_value = 0
        underscore_pos = index(text, '_')
        if (underscore_pos <= 0) return
        if (underscore_pos >= len_trim(text)) return

        suffix = trim(adjustl(text(underscore_pos + 1:)))
        if (len_trim(suffix) == 0) return
        read (suffix, *, iostat=ios) kind_value
        if (ios /= 0) kind_value = 0
    end function parse_literal_kind_suffix

    recursive logical function strict_types_match(expected, actual) result(matches)
        type(mono_type_t), intent(in) :: expected
        type(mono_type_t), intent(in) :: actual
        type(mono_type_t) :: expected_copy
        type(mono_type_t) :: actual_copy

        matches = .false.
        expected_copy = expected
        actual_copy = actual
        call expected_copy%sync_from_arena()
        call actual_copy%sync_from_arena()

        if (expected_copy%kind == TARRAY) then
            if (actual_copy%kind /= TARRAY) return
            if (.not. expected_copy%has_args()) return
            if (.not. actual_copy%has_args()) return
            matches = strict_types_match(expected_copy%get_arg(1), &
                                         actual_copy%get_arg(1))
            return
        end if

        matches = expected_copy%kind == actual_copy%kind
        if (matches .and. expected_copy%kind == TCHAR) then
            matches = expected_copy%size == actual_copy%size
        end if
    end function strict_types_match

    logical function strict_type_is_known(typ) result(is_known)
        type(mono_type_t), intent(in) :: typ
        type(mono_type_t) :: copy

        is_known = .false.
        copy = typ
        call copy%sync_from_arena()
        if (copy%kind <= 0) return
        if (copy%kind == TVAR) return
        is_known = .true.
    end function strict_type_is_known

    subroutine strict_type_name(typ, name)
        type(mono_type_t), intent(in) :: typ
        character(len=:), allocatable, intent(out) :: name
        type(mono_type_t) :: copy
        character(len=64) :: buffer

        copy = typ
        call copy%sync_from_arena()

        select case (copy%kind)
        case (TINT)
            name = "integer"
        case (TREAL)
            name = "real"
        case (TDOUBLE)
            name = "real(dp)"
        case (TLOGICAL)
            name = "logical"
        case (TCHAR)
            if (copy%size > 0) then
                write (buffer, '("character(len=", I0, ")")') copy%size
                name = trim(buffer)
            else
                name = "character"
            end if
        case (TCOMPLEX)
            name = "complex"
        case (TARRAY)
            name = "array"
        case default
            name = "unknown"
        end select
    end subroutine strict_type_name

end module semantic_strict_argument_type_checker_types
