module semantic_procedure_signature
    ! Reads a procedure definition out of the AST arena and reduces it to the
    ! small amount of signature information the F2003 type-bound-procedure
    ! override rules and the FINAL procedure rules need to compare: the dummy
    ! argument names, their declared type category and INTENT, and (for
    ! functions) the shape and character length of the result.
    !
    ! Only facts that are actually present in the AST are recorded. Every
    ! "known" flag stays .false. when the declaration could not be located, so
    ! callers can stay silent rather than guess. This matters because a wrong
    ! rejection makes a valid program uncompilable.
    use ast_arena_modern, only: ast_arena_t
    use ast_nodes_core, only: identifier_node
    use ast_nodes_data, only: declaration_node, parameter_declaration_node, &
        INTENT_IN, INTENT_OUT, INTENT_INOUT
    use ast_nodes_procedure, only: function_def_node, subroutine_def_node
    use string_utils_mod, only: to_lower
    implicit none
    private

    public :: dummy_info_t, procedure_signature_t
    public :: find_procedure_definition, build_procedure_signature
    public :: type_category

    type :: dummy_info_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: category
        character(len=:), allocatable :: intent_text
        logical :: category_known = .false.
        logical :: has_intent = .false.
        logical :: is_array = .false.
    end type dummy_info_t

    type :: procedure_signature_t
        logical :: found = .false.
        logical :: is_function = .false.
        character(len=:), allocatable :: name
        type(dummy_info_t), allocatable :: dummies(:)
        logical :: result_known = .false.
        character(len=:), allocatable :: result_category
        logical :: result_is_array = .false.
        logical :: result_has_char_len = .false.
        character(len=:), allocatable :: result_char_len
        integer :: line = 0
        integer :: column = 0
    end type procedure_signature_t

contains

    ! Locate the arena index of a FUNCTION or SUBROUTINE definition by name.
    ! Returns 0 when the name is not defined in this arena, and also 0 when the
    ! name is defined more than once, because an ambiguous match cannot be
    ! compared safely.
    function find_procedure_definition(arena, name) result(proc_index)
        type(ast_arena_t), intent(in) :: arena
        character(len=*), intent(in) :: name
        integer :: proc_index
        character(len=:), allocatable :: wanted
        integer :: i, matches

        proc_index = 0
        matches = 0
        if (len_trim(name) == 0) return
        wanted = to_lower(trim(name))

        do i = 1, arena%size
            if (.not. arena%has_node_at(i)) cycle
            select type (node => arena%entries(i)%node)
                type is (function_def_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) /= wanted) cycle
                matches = matches + 1
                proc_index = i
                type is (subroutine_def_node)
                if (.not. allocated(node%name)) cycle
                if (to_lower(trim(node%name)) /= wanted) cycle
                matches = matches + 1
                proc_index = i
            end select
        end do

        if (matches /= 1) proc_index = 0
    end function find_procedure_definition

    ! Reduce the procedure at proc_index to a comparable signature.
    function build_procedure_signature(arena, proc_index) result(sig)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: proc_index
        type(procedure_signature_t) :: sig

        if (proc_index <= 0) return
        if (.not. arena%has_node_at(proc_index)) return

        select type (node => arena%entries(proc_index)%node)
            type is (function_def_node)
            sig%found = .true.
            sig%is_function = .true.
            if (allocated(node%name)) sig%name = trim(node%name)
            sig%line = node%line
            sig%column = node%column
            call collect_dummies(arena, node%param_indices, node%body_indices, &
                sig%dummies)
            call collect_result(arena, node, sig)
            type is (subroutine_def_node)
            sig%found = .true.
            sig%is_function = .false.
            if (allocated(node%name)) sig%name = trim(node%name)
            sig%line = node%line
            sig%column = node%column
            call collect_dummies(arena, node%param_indices, node%body_indices, &
                sig%dummies)
        end select
    end function build_procedure_signature

    ! Build one dummy_info_t per dummy argument. Type and INTENT come from the
    ! parameter node itself when the parser produced an inline declaration, and
    ! otherwise from the matching declaration in the procedure body.
    subroutine collect_dummies(arena, param_indices, body_indices, dummies)
        type(ast_arena_t), intent(in) :: arena
        integer, allocatable, intent(in) :: param_indices(:)
        integer, allocatable, intent(in) :: body_indices(:)
        type(dummy_info_t), allocatable, intent(out) :: dummies(:)
        integer :: i, decl_index

        if (.not. allocated(param_indices)) then
            allocate (dummies(0))
            return
        end if

        allocate (dummies(size(param_indices)))
        do i = 1, size(param_indices)
            dummies(i)%name = param_name(arena, param_indices(i))
            call describe_from_node(arena, param_indices(i), dummies(i))
            if (dummies(i)%category_known) cycle
            if (len_trim(dummies(i)%name) == 0) cycle
            if (.not. allocated(body_indices)) cycle
            decl_index = find_declaration(arena, body_indices, dummies(i)%name)
            if (decl_index <= 0) cycle
            call describe_from_node(arena, decl_index, dummies(i))
        end do
    end subroutine collect_dummies

    ! Record type category, INTENT and rank from a declaration-carrying node.
    subroutine describe_from_node(arena, node_index, info)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: node_index
        type(dummy_info_t), intent(inout) :: info

        if (node_index <= 0) return
        if (.not. arena%has_node_at(node_index)) return

        select type (node => arena%entries(node_index)%node)
            type is (declaration_node)
            if (allocated(node%type_name)) then
                info%category = type_category(node%type_name)
                info%category_known = len_trim(info%category) > 0
            end if
            info%is_array = node%is_array
            if (node%has_intent) then
                if (allocated(node%intent)) then
                    info%intent_text = to_lower(trim(node%intent))
                    info%has_intent = len_trim(info%intent_text) > 0
                end if
            end if
            type is (parameter_declaration_node)
            if (allocated(node%type_name)) then
                info%category = type_category(node%type_name)
                info%category_known = len_trim(info%category) > 0
            end if
            info%is_array = node%is_array
            select case (node%intent_type)
            case (INTENT_IN)
                info%intent_text = 'in'
                info%has_intent = .true.
            case (INTENT_OUT)
                info%intent_text = 'out'
                info%has_intent = .true.
            case (INTENT_INOUT)
                info%intent_text = 'inout'
                info%has_intent = .true.
            end select
        end select
    end subroutine describe_from_node

    ! Describe the function result: the declaration of the RESULT variable, or
    ! of the function name when no RESULT clause is present.
    subroutine collect_result(arena, node, sig)
        type(ast_arena_t), intent(in) :: arena
        type(function_def_node), intent(in) :: node
        type(procedure_signature_t), intent(inout) :: sig
        character(len=:), allocatable :: result_name
        integer :: decl_index

        ! The parser allocates result_variable to an empty string when the
        ! function header carries no RESULT clause; the result is then declared
        ! under the function's own name.
        result_name = ''
        if (allocated(node%result_variable)) result_name = trim(node%result_variable)
        if (len_trim(result_name) == 0) then
            if (.not. allocated(node%name)) return
            result_name = trim(node%name)
        end if
        if (len_trim(result_name) == 0) return
        if (.not. allocated(node%body_indices)) return

        decl_index = find_declaration(arena, node%body_indices, result_name)
        if (decl_index <= 0) return
        if (.not. arena%has_node_at(decl_index)) return

        select type (decl => arena%entries(decl_index)%node)
            type is (declaration_node)
            if (.not. allocated(decl%type_name)) return
            sig%result_category = type_category(decl%type_name)
            sig%result_known = len_trim(sig%result_category) > 0
            sig%result_is_array = decl%is_array
            sig%result_has_char_len = decl%has_character_length
            if (allocated(decl%character_length_expr)) then
                sig%result_char_len = trim(decl%character_length_expr)
            end if
        end select
    end subroutine collect_result

    ! Reduce a declared type specification to the part the override rules care
    ! about. Kind and length selectors are dropped so that `integer` and
    ! `integer(4)` compare equal; TYPE and CLASS keep the derived type name so
    ! that `class(base_t)` and `class(r_t)` compare unequal.
    function type_category(type_name) result(category)
        character(len=*), intent(in) :: type_name
        character(len=:), allocatable :: category
        character(len=:), allocatable :: lowered, base, inner
        integer :: paren

        category = ''
        lowered = to_lower(trim(adjustl(type_name)))
        if (len_trim(lowered) == 0) return

        paren = index(lowered, '(')
        if (paren <= 0) then
            category = squeeze(lowered)
            return
        end if

        base = squeeze(lowered(1:paren - 1))
        if (base /= 'type' .and. base /= 'class') then
            category = base
            return
        end if

        inner = lowered(paren + 1:)
        paren = index(inner, ')', back=.true.)
        if (paren > 0) inner = inner(1:paren - 1)
        paren = index(inner, '(')
        if (paren > 0) inner = inner(1:paren - 1)
        paren = index(inner, ',')
        if (paren > 0) inner = inner(1:paren - 1)
        category = base//':'//squeeze(inner)
    end function type_category

    ! Remove every blank from text so that spacing differences in a declared
    ! type specification do not produce a spurious mismatch.
    function squeeze(text) result(packed)
        character(len=*), intent(in) :: text
        character(len=:), allocatable :: packed
        integer :: i

        packed = ''
        do i = 1, len(text)
            if (text(i:i) == ' ') cycle
            packed = packed//text(i:i)
        end do
    end function squeeze

    ! Extract the dummy argument name from a parameter node.
    function param_name(arena, param_index) result(name)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: param_index
        character(len=:), allocatable :: name

        name = ''
        if (param_index <= 0) return
        if (.not. arena%has_node_at(param_index)) return

        select type (node => arena%entries(param_index)%node)
            type is (identifier_node)
            if (allocated(node%name)) name = trim(node%name)
            type is (declaration_node)
            if (allocated(node%var_name)) name = trim(node%var_name)
            type is (parameter_declaration_node)
            if (allocated(node%name)) name = trim(node%name)
        end select
    end function param_name

    ! Find the body declaration that declares var_name, if any.
    function find_declaration(arena, body_indices, var_name) result(decl_index)
        type(ast_arena_t), intent(in) :: arena
        integer, intent(in) :: body_indices(:)
        character(len=*), intent(in) :: var_name
        integer :: decl_index
        character(len=:), allocatable :: wanted
        integer :: i, j

        decl_index = 0
        wanted = to_lower(trim(var_name))
        if (len_trim(wanted) == 0) return

        do i = 1, size(body_indices)
            if (.not. arena%has_node_at(body_indices(i))) cycle
            select type (node => arena%entries(body_indices(i))%node)
                type is (declaration_node)
                if (allocated(node%var_name)) then
                    if (to_lower(trim(node%var_name)) == wanted) then
                        decl_index = body_indices(i)
                        return
                    end if
                end if
                if (.not. allocated(node%var_names)) cycle
                do j = 1, size(node%var_names)
                    if (to_lower(trim(node%var_names(j))) == wanted) then
                        decl_index = body_indices(i)
                        return
                    end if
                end do
            end select
        end do
    end function find_declaration

end module semantic_procedure_signature
